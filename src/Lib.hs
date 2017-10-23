{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Lib where

import GHC.Generics
import Data.Aeson
import Data.Aeson.Types (Parser, parse)

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

msft15 = "GET https://www.alphavantage.co/query?function=TIME_SERIES_INTRADAY&symbol=MSFT&interval=1min&apikey="

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
  parseJSON = withObject "Tick" $ \tick ->
    Tick
    <$> tick .: "1. open"
    <*> tick .: "2. high"
    <*> tick .: "3. low"
    <*> tick .: "4. close"
    <*> tick .: "5. volume"

data TimeSeriesResponse =
  TimeSeriesResponse { --metaData :: Map String AlphaMetaData
                     --,
  ticks :: Map String (Map UTCTime Tick)
                     } deriving (Generic, Show)

httpExample :: IO ()
httpExample = do
    response <- httpJSON "POST http://httpbin.org/post"

    putStrLn $ "The status code was: " ++
               show (getResponseStatusCode response)
    print $ getResponseHeader "Content-Type" response
    S8.putStrLn $ Yaml.encode (getResponseBody response :: Value)

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

ticksParser :: Object -> Parser (Maybe (Map LocalTime Tick))
ticksParser wholeObject = wholeObject .:? "Time Series (1min)"

--instance FromJSON (Maybe (Map LocalTime Tick)) where
instance FromJSON TimeSeriesResponse where
  -- parseJSON :: Object -> Parser (Maybe (Map LocalTime Tick))
  parseJSON (Object object) = undefined
    -- fmap (\myb -> fmap TimeSeriesResponse myb) (ticksParser object)

decodedMinute = do
  file <- LS.readFile "sample/one_minute.json" :: IO LS.ByteString
  -- maybeWholeOb :: Maybe Object
  maybeWholeOb <- pure $ decode file :: IO (Maybe Object)
  let nested = maybeWholeOb >>= (\wholeOb -> Just $ parse ticksParser wholeOb)
      nested' = nested >>= (\result -> case result of
                 (Error _) -> Nothing
                 (Success x) -> (Just x)
             )
  return $ join nested'

someFunc :: IO (Map LocalTime (Tick))
someFunc = do
  responseValue <- httpJSON msft15 :: IO (Response Value)
  let
    body :: Object -- possible runtime error if match fails
    (Object body) = getResponseBody responseValue
    timeSeriesVal :: Value
    timeSeriesVal = body ! "Time Series (1min)"
    resultTimeSeries :: Result (Map LocalTime Tick)
    resultTimeSeries = fromJSON timeSeriesVal
  case resultTimeSeries of
    (Error _) -> return empty
    (Success mp) -> return mp
  
  

  -- maybeWholeOb <- pure $ decode body :: IO (Maybe Object)
  -- let nested = maybeWholeOb >>= (\wholeOb -> Just $ parse ticksParser wholeOb)
  --     nested' = nested >>= (\result -> case result of
  --                (Error _) -> Nothing
  --                (Success x) -> (Just x)
  --            )
  -- return $ join nested'
