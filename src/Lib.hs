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

import Types.Exchange
import qualified Types.Stock as Stock

import Types.AlphaRequest
import Types.AlphaResponse
import Types.AlphaResponse.JSON

import Types.Exchange.Psql (nasdaq, insertExchange)
import Types.Stock.Psql (insertStock, getStocks)

import Types.MostRecentTick.Redis
import Types.Tick.Psql (insertTicks, insertTicksSafe)

import DB.Psql
import DB.Redis (getRedisConnection, closeRedisConnection)

import Database.Redis

retrieveAlphaResponse :: Exchange -> Stock.Stock -> Request -> IO AlphaResponse
retrieveAlphaResponse exchange stock requestURI = do
  responseFAlphaResponse <- httpJSON requestURI :: IO (Response (Exchange -> Stock.Stock -> AlphaResponse))
  let
    fAlphaResponse :: (Exchange -> Stock.Stock -> AlphaResponse)
    fAlphaResponse = getResponseBody responseFAlphaResponse

    alphaResponse = fAlphaResponse exchange stock

  return alphaResponse


retrieveStocks :: IO ([Stock.Stock])
retrieveStocks = do
  psqlConn <- getPsqlConnection "conf/collector.yaml"
  stocks <- reverse <$> getStocks psqlConn :: IO [Stock.Stock]
  closePsqlConnection psqlConn
  return stocks


-- https://hackage.haskell.org/package/base-4.10.0.0/docs/Control-Concurrent.html#g:12
retrieveAndInsertStockTicks :: Int -> Stock.Stock -> IO ()
retrieveAndInsertStockTicks udelay stock = do
    request <- simpleFullRequest stock

    threadDelay udelay
    
    putStrLn $ show (Stock.stockId stock) ++ " " ++ (Stock.symbol stock) ++ "  retrieve "
    
    alphaResponse <- retrieveAlphaResponse (Stock.exchange stock) stock request

    putStrLn $ show (Stock.stockId stock) ++ " " ++ (Stock.symbol stock) ++ " ticks retrieved: " ++ (show (length (ticks alphaResponse)))

    psqlConn <- getPsqlConnection "conf/collector.yaml"            
            
    rowsInserted <- insertTicksSafe (ticks alphaResponse) psqlConn

    putStrLn $ show (Stock.stockId stock) ++ " " ++ (Stock.symbol stock) ++ " rows inserted: " ++ (show rowsInserted)

    closePsqlConnection psqlConn

    let lastTick = getLastTick alphaResponse

    redisConn <- getRedisConnection "conf/collector.yaml"

    runRedis redisConn (setTickTimestamp lastTick)

    putStrLn $ show (Stock.stockId stock) ++ " " ++ (Stock.symbol stock) ++ " most recent tick timestamp updated "

    -- TODO handle error
    void $ closeRedisConnection redisConn



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

multiplier :: Int
multiplier = 4

retrieveAndInsertStocksTicks :: [Stock.Stock] -> IO ()
retrieveAndInsertStocksTicks stocks = do
  
  mapM_ (\stock -> putStrLn $ (show (Stock.stockId stock)) ++ "  " ++ (Stock.symbol stock)) stocks

  putStrLn "----------------------"

  mapM_ (\stock -> do
            delay <- randomRIO (1, multiplier * (length stocks))

            let
              mdelay :: Double
              mdelay = (fromInteger (toInteger delay)) / 60
              udelay :: Int
              udelay = delay * 1000000

            putStrLn $ (Stock.symbol stock) ++ "  delay (minutes): " ++ show mdelay


            void $ forkChild $ retrieveAndInsertStockTicks udelay stock
        ) stocks

  waitForChildren

retrieveStocksAndInsertTicks :: IO ()
retrieveStocksAndInsertTicks = retrieveStocks >>= retrieveAndInsertStocksTicks

retrieveNStocksAndInsertTicks :: Int -> IO ()
retrieveNStocksAndInsertTicks n = ((\l -> take n l) <$> retrieveStocks) >>= retrieveAndInsertStocksTicks




