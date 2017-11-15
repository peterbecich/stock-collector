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

import Data.HashMap.Lazy ((!))

import Control.Monad
import Data.Functor

import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString as BS

import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as LS

import Network.HTTP.Simple

import AlphaRequest

import Types.AlphaMetaData
import Types.Tick
import Types.TimeSeriesResponse

import Psql

decodedMinute :: IO (Maybe (Map LocalTime Tick))
decodedMinute = do
  file <- LS.readFile "sample/one_minute.json" :: IO LS.ByteString
  maybeWholeOb <- pure $ decode file :: IO (Maybe Object)
  return $ do
    wholeOb <- maybeWholeOb
    ticks <- parseMaybe (\ob -> ob .: "Time Series (1min)") wholeOb
    ticks


instance Eq ZonedTime where
  (==) zt1 zt2 = (zonedTimeToUTC zt1) == (zonedTimeToUTC zt2)
  
instance Ord ZonedTime where
  compare zt1 zt2 = compare (zonedTimeToUTC zt1) (zonedTimeToUTC zt2)

retrieveTimeSeriesResponse :: Request -> IO TimeSeriesResponse
retrieveTimeSeriesResponse url = do
  responseValue <- httpJSON url :: IO (Response Value)
  let
    -- possible runtime error if match fails
    (Object body) = getResponseBody responseValue
    metaDataVal :: Value
    metaDataVal = body ! "Meta Data"
    metaDataResult = fromJSON metaDataVal :: Result AlphaMetaData
    (Success metaData) = metaDataResult  -- TODO unsafe unpack!
    timeSeriesVal :: Value
    timeSeriesVal = body ! "Time Series (1min)"
    (Object timeSeriesOb) = timeSeriesVal-- TODO also unsafe
    zonedTime' :: LocalTime -> ZonedTime
    zonedTime' localTime = ZonedTime localTime (timeZone metaData)
    timeSeriesResult :: Result (Map ZonedTime Tick)
    timeSeriesResult = fmap (\mp -> mapKeys zonedTime' mp) (parse ticksParser body)
  case (metaDataResult, timeSeriesResult) of -- error-prone
    (Success metaData, Success ticks) ->
      return $ TimeSeriesResponse metaData ticks
    (Success metaData, _) ->
      return $ TimeSeriesResponse metaData empty
    (_, _) -> undefined

-- TODO come back here!  not safe yet
safeRetrieve :: Request -> IO (Maybe TimeSeriesResponse)
safeRetrieve url = Just <$> (retrieveTimeSeriesResponse url)

example = do
  msft <- exampleRequest
  timeSeriesResponse <- retrieveTimeSeriesResponse msft
  let tks = ticks timeSeriesResponse
      numTicks = size tks
  putStrLn $ "number of ticks: " ++ (show numTicks)
  -- putStrLn $ show timeSeriesResponse
  mapM_ (putStrLn . show) (toList (ticks timeSeriesResponse))
  -- mapM_ (\(k, v) -> insertTick k v) (toList (ticks timeSeriesResponse))
  insertTicksCassandra timeSeriesResponse

failedExample = do
  bogus <- badRequest -- fails with runtime error
  -- json key doesn't exist
  timeSeriesResponse <- retrieveTimeSeriesResponse bogus
  -- let tks = ticks timeSeriesResponse
  --     numTicks = size tks
  -- putStrLn $ "number of ticks: " ++ (show numTicks)
  putStrLn $ show timeSeriesResponse
  mapM_ (putStrLn . show) (toList (ticks timeSeriesResponse))

exampleSteel = do
  steel <- exampleRequestSteel
  timeSeriesResponse <- retrieveTimeSeriesResponse steel
  let tks = ticks timeSeriesResponse
      numTicks = size tks
  putStrLn $ "number of ticks: " ++ (show numTicks)
  -- putStrLn $ show timeSeriesResponse
  mapM_ (putStrLn . show) (toList (ticks timeSeriesResponse))


tickers = ["GOOG", "MSFT", "AAPL", "FB"]

insertTickerCassandra :: String -> IO ()
insertTickerCassandra tickerSymbol = do
  key <- getKey
  request <- formatRequest tickerSymbol (intervals !! 0) Full key
  timeSeriesResponse <- retrieveTimeSeriesResponse request
  let tks = ticks timeSeriesResponse
      numTicks = size tks
  putStrLn $ "number of ticks: " ++ (show numTicks)
  insertTicksCassandra timeSeriesResponse


getTickersCassandra :: IO ()
getTickersCassandra = mapM_ insertTickerCassandra tickers


insertTickerPostgres :: String -> IO ()
insertTickerPostgres tickerSymbol = do
  key <- getKey
  request <- formatRequest tickerSymbol (Interval "1min") Full key
  timeSeriesResponse <- retrieveTimeSeriesResponse request
  let
    tks :: Map ZonedTime Tick
    tks = ticks timeSeriesResponse
    numTicks = size tks
  putStrLn $ "number of ticks: " ++ (show numTicks)
  postgresConnection <- getPsqlConnection
  rowsInserted <- insertTicks tickerSymbol tks postgresConnection
  putStrLn $ "number of rows inserted: " ++ (show rowsInserted)


getTickersPostgres :: IO ()
getTickersPostgres = do
  tickerSymbols <- getTickerSymbols "conf/fortune500tickers.txt"
  -- tickerSymbols <- getTickerSymbols "conf/tenTickers.txt"
  putStrLn $ show tickerSymbols
  delays <- foldM (\map tickerSymbol -> do
                      delay <- randomRIO (1, 600)
                      let
                        udelay :: Int
                        udelay = delay * 1000000
                      return $ insert tickerSymbol udelay map
                  ) empty tickerSymbols
  putStrLn $ show $ assocs delays
  
  mapM_ (\tickerSymbol -> void $ forkIO $ do
            let udelay = (Map.!) delays tickerSymbol
            threadDelay udelay
            putStrLn $ "Delay expired. Retrieve and insert " ++ tickerSymbol
            insertTickerPostgres tickerSymbol
        ) tickerSymbols
