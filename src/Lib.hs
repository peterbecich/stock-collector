{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Lib
    ( someFunc
    ) where

import GHC.Generics
import Data.Aeson
-- import qualified Data.Yaml as Yaml

import Data.Time.Clock
import Data.Time.LocalTime

import Data.Map (Map, empty)

import Control.Monad
import Data.Functor

import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString as BS

import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as LS

import Network.HTTP.Simple

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

instance FromJSON Tick where
  parseJSON val = withObject "Tick" (\tick -> Tick
                                      <$> tick .: "1. open"
                                      <*> tick .: "2. high"
                                      <*> tick .: "3. low"
                                      <*> tick .: "4. close"
                                      <*> tick .: "5. volume"
                                    ) val

-- instance FromJSON (Map LocalTime Tick)

data TimeSeriesResponse =
  TimeSeriesResponse { metaData :: Map String AlphaMetaData
                     , ticks :: Map String (Map UTCTime Tick)
                     } deriving (Generic, Show)

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
  return maybeTSR

decodedMetaData :: IO (Maybe (Map String AlphaMetaData))
decodedMetaData = do
  metaDataFile <- LS.readFile "sample/metadata.json"
  _ <- putStrLn $ L8.unpack metaDataFile
  _ <- putStrLn "---------------"
  return $ decode metaDataFile

decodedMetaData2 :: IO (Maybe AlphaMetaData)
decodedMetaData2 = do
  metaDataFile <- LS.readFile "sample/metadata2.json"
  _ <- putStrLn $ L8.unpack metaDataFile
  _ <- putStrLn "---------------"
  return $ decode metaDataFile

-- https://hackage.haskell.org/package/yaml-0.8.23.3/docs/Data-Yaml.html
decodedTicks :: IO (Maybe (Map String (Map LocalTime Tick)))
decodedTicks = do
  ticksFile <- LS.readFile "sample/ticks.json"
  _ <- putStrLn $ L8.unpack ticksFile
  _ <- putStrLn "-----------------"
  return $ decode ticksFile

decodedTicks2 :: IO (Maybe (Map String Tick))
decodedTicks2 = do
  ticksFile <- LS.readFile "sample/ticks2.json"
  _ <- putStrLn $ L8.unpack ticksFile
  _ <- putStrLn "-----------------"
  return $ decode ticksFile

-- https://artyom.me/aeson

--decodedTicks3-- :: IO (Maybe (Map String Tick))
decodedTicks3 = do
  ticksFile <- LS.readFile "sample/one_minute.json"
  maybeTicksObject <- pure $ decode ticksFile
  timeSeries <- pure $ maybeTicksObject >>= (\ticksObject -> ticksObject .:? "Time Series (1min)")
  return $ decode timeSeries

decodedTick :: IO (Maybe (Map LocalTime Tick))
decodedTick = do
  tickFile <- LS.readFile "sample/tick.json"
  _ <- putStrLn $ L8.unpack tickFile
  _ <- putStrLn "-------------------"
  return $ decode tickFile

decodedTick2 :: IO (Maybe Tick)
decodedTick2 = do
  tickFile <- LS.readFile "sample/tick2.json"
  _ <- putStrLn $ L8.unpack tickFile
  _ <- putStrLn "-------------------"
  return $ decode tickFile

decodedMinute :: IO (Maybe TimeSeriesResponse)
decodedMinute = do
  file <- LS.readFile "sample/one_minute.json"
  _ <- putStrLn $ L8.unpack file
  _ <- putStrLn "------------------"
  return $ decode file


