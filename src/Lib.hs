{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Lib where

import Control.Concurrent (threadDelay, forkIO)
import System.Random

import GHC.Generics
import Data.Aeson
import Data.Aeson.Types (Parser, parse, parseMaybe)

import Data.Time.Clock
import Data.Time.LocalTime

import Data.Map (Map, empty, size, mapKeys, toList, elems, insert, assocs)
import qualified Data.Map.Lazy as Map ((!)) 

import qualified Data.HashMap.Lazy as HMap ((!), keys)

import Control.Monad
import Data.Functor

import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString as BS

import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as LS

import Network.HTTP.Simple

import Types.Exchange
import Types.Stock

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

retrieveAlphaResponse :: Exchange -> Stock -> Request -> IO AlphaResponse
retrieveAlphaResponse exchange stock requestURI = do
  responseFAlphaResponse <- httpJSON requestURI :: IO (Response (Exchange -> Stock -> AlphaResponse))
  let
    fAlphaResponse :: (Exchange -> Stock -> AlphaResponse)
    fAlphaResponse = getResponseBody responseFAlphaResponse

    alphaResponse = fAlphaResponse exchange stock

  return alphaResponse


retrieveStocks :: IO ([Stock])
retrieveStocks = do
  psqlConn <- getPsqlConnection "conf/collector.yaml"
  stocks <- reverse <$> getStocks psqlConn :: IO [Stock]
  closePsqlConnection psqlConn
  return stocks
  
retrieveAndInsertStockTicks :: [Stock] -> IO ()
retrieveAndInsertStockTicks stocks = do
  
  mapM_ (\stock -> putStrLn $ (show (stockId stock)) ++ "  " ++ (symbol stock)) stocks

  putStrLn "----------------------"

  mapM_ (\stock -> forkIO $ do
            request <- simpleFullRequest stock
            --request <- simpleCompactRequest stock
            -- delay
            delay <- randomRIO (1, 4*(length stocks))
            let udelay :: Int
                udelay = delay * 1000000
            
            threadDelay udelay

            putStrLn $ "retrieve " ++ (symbol stock)
            alphaResponse <- retrieveAlphaResponse nasdaq stock request

            putStrLn $ (show (stockId stock)) ++ "  " ++ (symbol stock)
            psqlConn <- getPsqlConnection "conf/collector.yaml"            
            
            rowsInserted <- insertTicksSafe (ticks alphaResponse) psqlConn
            closePsqlConnection psqlConn

            let lastTick = getLastTick alphaResponse
            redisConn <- getRedisConnection "conf/collector.yaml"

            runRedis redisConn (setTickTimestamp lastTick)

            -- TODO handle error
            void $ closeRedisConnection redisConn
            
            putStrLn $ (symbol stock) ++ " rows inserted: " ++ (show rowsInserted)
        ) stocks


retrieveStocksAndInsertTicks :: IO ()
retrieveStocksAndInsertTicks = retrieveStocks >>= retrieveAndInsertStockTicks

retrieveNStocksAndInsertTicks :: Int -> IO ()
retrieveNStocksAndInsertTicks n = ((\l -> take n l) <$> retrieveStocks) >>= retrieveAndInsertStockTicks
  
