{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc
    ) where

import GHC.Generics
import Data.Aeson (Value)
import Data.Yaml as Yaml

import Data.Time.Clock
import Data.Time.LocalTime

import Data.Map.Lazy (Map, empty)

import Control.Monad
import Data.Functor

import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString as BS

import Network.HTTP.Simple

msft15 = "https://www.alphavantage.co/query?function=TIME_SERIES_INTRADAY&symbol=MSFT&interval=1min&apikey="

data AlphaMetaData = AlphaMetaData { info :: String
                                   , symbol :: String
                                   , lastRefresh :: LocalTime
                                   , interval :: String
                                   , outputSize :: String
                                   } deriving (Generic, Show)

instance ToJSON AlphaMetaData
instance FromJSON AlphaMetaData where
  parseJSON = withObject "AlphaMetaData" $ \amd -> AlphaMetaData
    <$> amd .: "1. Information"
    <*> amd .: "2. Symbol"
    <*> amd .: "3. Last Refreshed"
    <*> amd .: "4. Interval"
    <*> amd .: "5. Output Size"

data Tick = Tick { open :: Double
                 , high :: Double
                 , low :: Double
                 , close :: Double
                 , volume :: Int
                 } deriving (Generic, Show)

instance ToJSON Tick
instance FromJSON Tick where
  parseJSON val = withObject "Tick" (\tick -> Tick
                                      <$> tick .: "1. open"
                                      <*> tick .: "2. high"
                                      <*> tick .: "3. low"
                                      <*> tick .: "4. close"
                                      <*> tick .: "5. volume"
                                    ) val

data TimeSeriesResponse =
  TimeSeriesResponse { metaData :: AlphaMetaData
                     , ticks :: Map UTCTime Tick
                     } deriving (Generic, Show)

instance ToJSON TimeSeriesResponse
instance FromJSON TimeSeriesResponse where
  -- parseJSON :: Value -> Parser TimeSeriesResponse
  parseJSON = withObject "TimeSeriesResponse" $
    \tsr -> TimeSeriesResponse
            <$> tsr .: "Meta Data"
            <*> tsr .: "Time Series (1min)"

someFunc :: IO (Maybe TimeSeriesResponse)
someFunc = do
  response <- httpJSON msft15
  let
    maybeTSR :: Maybe TimeSeriesResponse
    maybeTSR = getResponseBody response
  return $ maybeTSR

decodedExample :: IO (Maybe TimeSeriesResponse)
decodedExample = do
  oneMinute <- BS.readFile "sample/one_minute.json"
  putStrLn $ S8.unpack oneMinute
  putStrLn "-----------------"
  maybeTSR <- pure $ Yaml.decode oneMinute
  return maybeTSR


-- https://hackage.haskell.org/package/yaml-0.8.23.3/docs/Data-Yaml.html
decodedTicks :: IO (Maybe (Map LocalTime Tick))
decodedTicks = do
  ticksFile <- BS.readFile "sample/ticks.json"
  _ <- putStrLn $ S8.unpack ticksFile
  _ <- putStrLn "-----------------"
  maybeTicks <- pure $ Yaml.decode ticksFile
  return maybeTicks

