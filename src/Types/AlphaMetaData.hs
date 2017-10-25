{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module Types.AlphaMetaData where

import GHC.Generics

import Data.Aeson
import Data.Aeson.Types (Parser, parse, parseMaybe, Value)

import Data.Time.LocalTime

data AlphaMetaData = AlphaMetaData { info :: String
                                   , symbol :: String
                                   , lastRefresh :: LocalTime
                                   , interval :: String
                                   , outputSize :: String
                                   , timeZone :: TimeZone
                                   } deriving (Generic, Show)

instance FromJSON AlphaMetaData where
  parseJSON = withObject "Meta Data" $ \amd -> AlphaMetaData
    <$> amd .: "1. Information"
    <*> amd .: "2. Symbol"
    <*> amd .: "3. Last Refreshed"
    <*> amd .: "4. Interval"
    <*> amd .: "5. Output Size"
    <*> amd .: "6. Time Zone"

-- TODO make dependent on date, DST
instance FromJSON TimeZone where
  parseJSON (String timeString)
    | timeString == "US/Eastern" =
      pure $ TimeZone 240 False "US/Eastern"
    | otherwise =
      pure $ TimeZone 0 False "UTC"  -- TODO


