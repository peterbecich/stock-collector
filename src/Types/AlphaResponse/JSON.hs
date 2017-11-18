{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Types.AlphaResponse.JSON where

import Data.Aeson
import qualified Data.Aeson as Aeson

import Data.Aeson.Types (Parser, parse, parseMaybe)

import qualified Data.Map as Mp --  (Map, empty, keys)
import Data.Text.Internal (Text)
import Data.ByteString.Lazy.Char8 (pack, unpack)

import Control.Monad

import Types.Exchange
import Types.Exchange.Psql (nasdaq)
import Types.Stock
import Types.Stock
import Types.Stock.Psql (bogusStock)
import Types.Tick
import Types.Tick.JSON
import Types.AlphaMetaData
import Types.AlphaMetaData.JSON

import Types.AlphaResponse

import Data.Time.Clock
import Data.Time.LocalTime

-- TODO don't assume exchange is NASDAQ!
-- instance FromJSON AlphaResponse where
--   parseJSON = withObject "Foo" $ \response -> do
--     metaData <- (response .: "Meta Data") :: Parser AlphaMetaData
--     -- TODO handle different intervals
--     timeSeries <- (response .: "Time Series (1min)") :: Parser (Mp.Map LocalTime (UTCTime -> Stock -> Tick))

--     let
--       timeZone :: TimeZone
--       timeZone = TimeZone (-300) False "US/Eastern"

--       timeSeriesTransformed :: [Tick]
--       timeSeriesTransformed = transformTicksMap bogusStock timeZone timeSeries

--       alphaResponse = AlphaResponse nasdaq bogusStock timeSeriesTransformed
      
--     return alphaResponse


instance FromJSON (Exchange -> Stock -> AlphaResponse) where
  parseJSON = withObject "Foo" $ \response -> do
    metaData <- (response .: "Meta Data") :: Parser AlphaMetaData
    -- TODO handle different intervals
    timeSeries <- (response .: "Time Series (1min)") :: Parser (Mp.Map LocalTime (UTCTime -> Stock -> Tick))

    let
      timeZone :: TimeZone
      timeZone = TimeZone (-300) False "US/Eastern"

      timeSeriesTransformed :: [Tick]
      timeSeriesTransformed = transformTicksMap bogusStock timeZone timeSeries

      falphaResponse exchange stock = AlphaResponse exchange stock timeSeriesTransformed
      
    return falphaResponse



exampleResponseStr :: IO String
exampleResponseStr = readFile "sample/sampleResponse.json"

exampleDecodeAlphaResponse :: IO AlphaResponse
exampleDecodeAlphaResponse = do
  responseString <- pack <$> exampleResponseStr

  let
    eAlphaResponse :: Either String (Exchange -> Stock -> AlphaResponse)
    eAlphaResponse = eitherDecode responseString

    -- unsafe

    falphaResponse :: Exchange -> Stock -> AlphaResponse
    (Right falphaResponse) = eAlphaResponse

    alphaResponse = falphaResponse nasdaq bogusStock

  -- case eAlphaResponse of
  --   (Left err) -> putStrLn err
  --   (Right res) -> putStrLn $ show res

  return alphaResponse

