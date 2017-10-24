{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Collector where

import GHC.Generics
import Data.Aeson
import Data.Aeson.Types (Parser, parse, parseMaybe)

import Data.Time.Clock
import Data.Time.LocalTime

import Data.Map (Map, empty)

import Data.HashMap.Lazy ((!))

import Control.Monad
import Data.Functor

import qualified Data.Yaml as Yaml

import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString as BS

import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as LS

import Network.HTTP.Simple


import Collector.Types.AlphaMetaData
import Collector.Types.Tick
import Collector.Types.TimeSeriesResponse

msft15 :: Request
msft15 = "https://www.alphavantage.co/query?function=TIME_SERIES_INTRADAY&symbol=MSFT&interval=1min&apikey="
    


--decodedMinute :: IO (Maybe (Map LocalTime Tick'))
decodedMinute :: IO (Maybe (Map LocalTime Tick'))
decodedMinute = do
  file <- LS.readFile "sample/one_minute.json" :: IO LS.ByteString
  maybeWholeOb <- pure $ decode file :: IO (Maybe Object)
  return $ do
    wholeOb <- maybeWholeOb
    ticks <- parseMaybe (\ob -> ob .: "Time Series (1min)") wholeOb
    ticks

retrieveTimeSeriesResponse :: Request -> IO TimeSeriesResponse
retrieveTimeSeriesResponse url = do
  responseValue <- httpJSON url :: IO (Response Value)
  let
    -- possible runtime error if match fails
    (Object body) = getResponseBody responseValue
    metaDataVal :: Value
    metaDataVal = body ! "Meta Data"
    metaDataResult = fromJSON metaDataVal :: Result AlphaMetaData
    timeSeriesVal :: Value
    timeSeriesVal = body ! "Time Series (1min)"
    (Object timeSeriesOb) = timeSeriesVal
    timeSeriesResult :: Result (Map LocalTime Tick')
    timeSeriesResult = parse ticksParser' body
  case (metaDataResult, timeSeriesResult) of -- error-prone
    (Success metaData, Success ticks) ->
      return $ TimeSeriesResponse metaData ticks
    (Success metaData, _) ->
      return $ TimeSeriesResponse metaData empty
    (_, _) -> undefined
