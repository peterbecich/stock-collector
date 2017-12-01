{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Lib where

import Control.Concurrent
import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Par.IO
import Data.Aeson
import Data.Aeson.Types (Parser, parse, parseMaybe)
import Data.Int (Int64)
import Data.List
import Data.Functor
import Data.Map (Map, empty, size, mapKeys, toList, elems, insert, assocs)
import Data.Time.Clock
import Data.Time.LocalTime
import GHC.Generics
import Network.HTTP.Simple
import System.IO.Unsafe (unsafePerformIO)
import System.Random
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as LS
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.HashMap.Lazy as HMap ((!), keys)
import qualified Data.Map.Lazy as Map ((!)) 

import Database.Redis

import Types.Exchange
import qualified Types.Stock as Stock

import Types.AlphaRequest
import Types.AlphaResponse
import Types.AlphaResponse.JSON

import Types.Exchange.Psql (nasdaq, insertExchange)
import Types.Stock.Psql (insertStock, getStocks, stockQuery)

import Types.MostRecentTick.Redis
import Types.Tick.Psql (insertTicks, insertTicksSafe)

import DB.Psql
import DB.Redis (getRedisConnection, closeRedisConnection)

import Stats.StockCovariance (pairCovarianceNStocks, pairCovarianceStocks)

-- list of stocks
retrieveStocks :: IO ([Stock.Stock])
retrieveStocks = do
  psqlConn <- getPsqlConnection "conf/collector.yaml"
  stocks <- reverse <$> getStocks psqlConn :: IO [Stock.Stock]
  closePsqlConnection psqlConn
  return stocks

-- given stock, retrieve ticks from AlphaVantage and insert them into Postgres
-- https://hackage.haskell.org/package/base-4.10.0.0/docs/Control-Concurrent.html#g:12
-- retrieveAndInsertStockTicks :: Int -> Stock.Stock -> IO ()
-- retrieveAndInsertStockTicks udelay stock = do
--     request <- simpleFullRequest stock

--     threadDelay udelay
    
--     putStrLn $ show (Stock.stockId stock) ++ " " ++ (Stock.symbol stock) ++ "  retrieve "
    
--     alphaResponse <- retrieveAlphaResponse (Stock.exchange stock) stock request

--     putStrLn $ show (Stock.stockId stock) ++ " " ++ (Stock.symbol stock) ++ " ticks retrieved: " ++ (show (length (ticks alphaResponse)))

--     psqlConn <- getPsqlConnection "conf/collector.yaml"            
            
--     rowsInserted <- insertTicksSafe (ticks alphaResponse) psqlConn

--     putStrLn $ show (Stock.stockId stock) ++ " " ++ (Stock.symbol stock) ++ " rows inserted: " ++ (show rowsInserted)

--     closePsqlConnection psqlConn

--     let lastTick = getLastTick alphaResponse

--     redisConn <- getRedisConnection "conf/collector.yaml"

--     runRedis redisConn (setTickTimestamp lastTick)

--     putStrLn $ show (Stock.stockId stock) ++ " " ++ (Stock.symbol stock) ++ " most recent tick timestamp updated "

--     -- TODO handle error
--     void $ closeRedisConnection redisConn


-- retrieveAndInsertStocksTicks :: [Stock.Stock] -> IO ()
-- retrieveAndInsertStocksTicks stocks = do
  
--   mapM_ (\stock -> putStrLn $ (show (Stock.stockId stock)) ++ "  " ++ (Stock.symbol stock)) stocks

--   putStrLn "----------------------"

--   mapM_ (\stock -> do
--             delay <- randomRIO (1, multiplier * (length stocks))

--             let
--               mdelay :: Double
--               mdelay = (fromInteger (toInteger delay)) / 60
--               udelay :: Int
--               udelay = delay * 1000000

--             putStrLn $ (Stock.symbol stock) ++ "  delay (minutes): " ++ show mdelay


--             void $ forkChild $ retrieveAndInsertStockTicks udelay stock
--         ) stocks

--   waitForChildren

-- retrieveStocksAndInsertTicks :: IO ()
-- retrieveStocksAndInsertTicks = do
--   stocks <- retrieveStocks
--   retrieveAndInsertStocksTicks stocks
--   redisConn <- getRedisConnection "conf/collector.yaml"
--   psqlConn <- getPsqlConnection "conf/collector.yaml"            
  
--   pairCovarianceStocks redisConn psqlConn stocks
--   putStrLn "done calculating covariance pairs"
--   void $ closeRedisConnection redisConn
--   closePsqlConnection psqlConn

-- retrieveNStocksAndInsertTicks :: Int -> IO ()
-- retrieveNStocksAndInsertTicks n = do
--   stocks <- ((\l -> take n l) <$> retrieveStocks)
--   retrieveAndInsertStocksTicks stocks
--   redisConn <- getRedisConnection "conf/collector.yaml"
--   psqlConn <- getPsqlConnection "conf/collector.yaml"            

--   putStrLn $ show (length stocks) ++ " stocks to calculate covariances for"
--   pairCovarianceStocks redisConn psqlConn stocks

--   void $ closeRedisConnection redisConn
--   closePsqlConnection psqlConn


----------------------------------------------

-- simple method to keep parent thread from finishing before all child threads are finished
-- https://hackage.haskell.org/package/base-4.10.0.0/docs/Control-Concurrent.html#g:12
children :: MVar [MVar ()]
children = unsafePerformIO (newMVar [])

waitForChildren :: IO ()
waitForChildren = do
  cs <- takeMVar children
  case cs of
    [] -> return ()
    m:ms -> do
      putMVar children ms
      takeMVar m
      waitForChildren

forkChild :: IO () -> IO ThreadId
forkChild io = do
  mvar <- newEmptyMVar
  childs <- takeMVar children
  putMVar children (mvar:childs)
  forkFinally io (\_ -> putMVar mvar ())

-- multiplier on delay between AlphaVantage queries
multiplier :: Int
multiplier = 4

randomDelay :: Int -> IO ()
randomDelay n = do
  delay <- randomRIO (1, multiplier * n)
  let
    mdelay :: Double
    mdelay = (fromInteger (toInteger delay)) / 60
    udelay :: Int
    udelay = delay * 1000000
  putStrLn $ show (round mdelay) ++ " minutes delay"
  threadDelay udelay

confPath :: FilePath
confPath = "conf/collector.yaml"

-- given stock, hit AlphaVantage REST endpoint
retrieveAlphaResponse :: Stock.Stock -> Request -> IO AlphaResponse
retrieveAlphaResponse stock requestURI = do
  responseFAlphaResponse <- httpJSON requestURI :: IO (Response (Exchange -> Stock.Stock -> AlphaResponse))
  let
    fAlphaResponse :: (Exchange -> Stock.Stock -> AlphaResponse)
    fAlphaResponse = getResponseBody responseFAlphaResponse

    alphaResponse = fAlphaResponse (Stock.exchange stock) stock

  return alphaResponse

insertAlphaResponse :: PostgresPool -> AlphaResponse -> IO Int64
insertAlphaResponse pool alphaResponse = do
  insertTicksSafe pool (ticks alphaResponse)
  
  
collectNStockTicks :: Int -> IO ()
collectNStockTicks n = do
  pool <- createPostgresPool confPath

  stocks <- runQueryPool pool stockQuery :: IO [Stock.Stock]

  let stocks' = take n stocks

  putStrLn $ show (length stocks') ++ " stocks to retrieve ticks for"

  mapM_ (\stock -> void $ forkChild $ do
            randomDelay (length stocks')
            request <- simpleFullRequest stock
            -- putStrLn $ show request
            alphaResponse <- retrieveAlphaResponse stock request
            putStrLn $ "retrieved " ++ show (length (ticks alphaResponse)) ++ " ticks"
            rowsInserted <- insertAlphaResponse pool alphaResponse
            putStrLn $ "inserted " ++ show rowsInserted ++ " rows"
        ) stocks'

  waitForChildren
  
  return ()

collectStockTicks :: IO ()
collectStockTicks = do
  pool <- createPostgresPool confPath

  stocks <- runQueryPool pool stockQuery :: IO [Stock.Stock]

  putStrLn $ show (length stocks) ++ " stocks to retrieve ticks for"

  mapM_ (\stock -> void $ forkChild $ do
            randomDelay (length stocks)
            request <- simpleFullRequest stock
            -- putStrLn $ show request
            alphaResponse <- retrieveAlphaResponse stock request
            putStrLn $ "retrieved " ++ show (length (ticks alphaResponse)) ++ " ticks"
            rowsInserted <- insertAlphaResponse pool alphaResponse
            putStrLn $ "inserted " ++ show rowsInserted ++ " rows"
        ) stocks

  waitForChildren
  
  return ()
