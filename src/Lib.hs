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
import Data.Either
import Data.Functor
import Data.Map (Map, empty, size, mapKeys, toList, elems, insert, assocs)
import Data.Time.Clock
import Data.Time.LocalTime
import GHC.Generics
import Network.HTTP.Simple
import System.IO
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

import qualified Types.Tick as Tick

import DB.Psql
import DB.Redis

import Stats.StockCovariance (pairCovarianceNStocks, pairCovarianceStocks)

-- list of stocks
retrieveStocks :: IO ([Stock.Stock])
retrieveStocks = do
  psqlConn <- getPsqlConnection "conf/collector.yaml"
  stocks <- reverse <$> getStocks psqlConn :: IO [Stock.Stock]
  closePsqlConnection psqlConn
  return stocks

-- https://hackage.haskell.org/package/base-4.10.0.0/docs/Control-Concurrent.html#g:12
-- children :: MVar [MVar ()]
-- children = unsafePerformIO (newMVar [])

-- waitForChildren :: IO ()
-- waitForChildren = do
--   cs <- takeMVar children
--   case cs of
--     [] -> return ()
--     m:ms -> do
--       putMVar children ms
--       takeMVar m
--       waitForChildren

-- forkChild :: IO () -> IO ThreadId
-- forkChild io = do
--   mvar <- newEmptyMVar
--   childs <- takeMVar children
--   putMVar children (mvar:childs)
--   forkFinally io (\_ -> putMVar mvar ())

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
confPath = "/usr/local/etc/collector.yaml"
--confPath = "conf/collector.yaml"

-- given stock, hit AlphaVantage REST endpoint
-- https://hackage.haskell.org/package/http-conduit-2.2.4/docs/Network-HTTP-Simple.html
retrieveAlphaResponse :: Stock.Stock -> Request -> IO (Maybe AlphaResponse)
retrieveAlphaResponse stock requestURI = do
  responseFAlphaResponse <- httpJSONEither requestURI :: IO (Response (Either JSONException (Exchange -> Stock.Stock -> AlphaResponse)))
  let
    fAlphaResponse :: Either JSONException (Exchange -> Stock.Stock -> AlphaResponse)
    fAlphaResponse = getResponseBody responseFAlphaResponse

    exchange = Stock.exchange stock

    mybe :: Maybe (Exchange -> Stock.Stock -> AlphaResponse)
    mybe = either (\_ -> Nothing) (Just) fAlphaResponse
    
  return $ (\f -> f exchange stock) <$> mybe



insertAlphaResponse :: PostgresPool -> AlphaResponse -> IO Int64
insertAlphaResponse pool alphaResponse = do
  insertTicksSafe pool (ticks alphaResponse)
  
collectStockTicks :: IO ()
collectStockTicks = do
  putStrLn "sleep for 30 seconds to allow stocks to be loaded into Postgres"
  threadDelay 30000000
  
  threads <- getNumCapabilities
  putStrLn $ show threads ++ " threads"
  
  psqlPool <- createPostgresPool confPath
  redisPool <- createRedisPool confPath

  stocks <- runQueryPool psqlPool stockQuery :: IO [Stock.Stock]

  putStrLn $ show (length stocks) ++ " stocks to retrieve ticks for"

  -- no parallelism until throttling issue is solved
  -- https://gist.github.com/roman/5578320
  --void $ forkChild $ do
  mapM_ (\stock -> do
            putStrLn $ Stock.symbol stock
            hPutStrLn stdout $ Stock.symbol stock
            request <- simpleFullRequest stock
            malphaResponse <- retrieveAlphaResponse stock request
            case malphaResponse of
              Nothing -> return ()
              (Just alphaResponse) -> do  
                putStrLn $ (Stock.symbol stock) ++": retrieved " ++ show (length (ticks alphaResponse)) ++ " ticks"
                rowsInserted <- insertAlphaResponse psqlPool alphaResponse
                putStrLn $ (Stock.symbol stock) ++ ": inserted " ++ show rowsInserted ++ " rows"
                let lastTick = getLastTick alphaResponse -- TODO improve
                runRedisPool redisPool (setTickTimestamp lastTick)
                putStrLn $ (Stock.symbol stock) ++ ": set latest timestamp " ++ show (Tick.time lastTick)
        ) stocks

  -- waitForChildren

  putStrLn "done"
  return ()
