{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Lib where

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

msft15 :: Request
msft15 = "https://www.alphavantage.co/query?function=TIME_SERIES_INTRADAY&symbol=MSFT&interval=1min&apikey="

data AlphaMetaData = AlphaMetaData { info :: String
                                   , symbol :: String
                                   , lastRefresh :: LocalTime
                                   , interval :: String
                                   , outputSize :: String
                                   } deriving (Generic, Show)

instance FromJSON AlphaMetaData where
  parseJSON = withObject "Meta Data" $ \amd -> AlphaMetaData
    <$> amd .: "1. Information"
    <*> amd .: "2. Symbol"
    <*> amd .: "3. Last Refreshed"
    <*> amd .: "4. Interval"
    <*> amd .: "5. Output Size"

data Tick = Tick { open :: String
                 , high :: String
                 , low :: String
                 , close :: String
                 , volume :: String
                 } deriving (Generic, Show)


data Tick' = Tick' { open' :: Double
                 , high' :: Double
                 , low' :: Double
                 , close' :: Double
                 , volume' :: Int
                 } deriving (Generic, Show)


instance FromJSON Tick where
  parseJSON = withObject "Tick" $ \tick -> do
    open <- tick .: "1. open"
    high <- tick .: "2. high"
    low <- tick .: "3. low"
    close <- tick .: "4. close"
    volume <- tick .: "5. volume"
    return $ Tick open high low close volume

instance FromJSON Tick' where  -- unsafe !!!
  parseJSON = withObject "Tick" $ \tick -> do
    open <- read <$> tick .: "1. open"
    high <- read <$> tick .: "2. high"
    low <- read <$> tick .: "3. low"
    close <- read <$> tick .: "4. close"
    volume <- read <$> tick .: "5. volume"
    return $ Tick' open high low close volume
    

data TimeSeriesResponse =
  TimeSeriesResponse { metaData :: AlphaMetaData
                     , ticks :: Map LocalTime Tick'
                     } deriving (Generic, Show)

ticksParser :: Object -> Parser (Map LocalTime Tick)
ticksParser wholeObject = wholeObject .: "Time Series (1min)"

ticksParser' :: Object -> Parser (Map LocalTime Tick')
ticksParser' wholeObject = wholeObject .: "Time Series (1min)"

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
