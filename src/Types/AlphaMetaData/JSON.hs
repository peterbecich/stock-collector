{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Types.AlphaMetaData.JSON where

import Data.Aeson
import Data.Aeson.Types (Parser, parse, parseMaybe, Value)
import Data.Time.LocalTime
import Data.Text.Internal (Text)
import Data.ByteString.Lazy.Char8 (pack, unpack)

import Types.AlphaMetaData

-- https://hackage.haskell.org/package/aeson-1.2.3.0/docs/Data-Aeson.html

instance ToJSON AlphaMetaData where
  --toEncoding = genericToEncoding defaultOptions
  toJSON (AlphaMetaData info symbol lastRefresh interval outputSize timeZone) =
    object [ "1. Information" .= info
           , "2. Symbol" .= symbol
           , "3. Last Refreshed" .= lastRefresh
           , "4. Interval" .= interval
           , "5. Output Size" .= outputSize
           , "6. Time Zone" .= (String "US/Eastern") -- TODO improve
           ]

instance FromJSON AlphaMetaData where
  parseJSON = withObject "Meta Data" $ \amd -> AlphaMetaData
    <$> amd .: "1. Information"
    <*> amd .: "2. Symbol"
    <*> amd .: "3. Last Refreshed"
    <*> amd .: "4. Interval"
    <*> amd .: "5. Output Size"
    <*> amd .: "6. Time Zone"

-- TODO make dependent on date, DST
instance ToJSON TimeZone where
  toJSON :: TimeZone -> Value
  toJSON timeZone =
    let
      k :: Text
      k = "6. Time Zone"
      v :: Text
      v = "US/Eastern"
    in object [k .= v] -- TODO fix!

instance FromJSON TimeZone where
  parseJSON (String timeString)
    | timeString == "US/Eastern" =
      pure $ TimeZone 240 False "US/Eastern"
    | otherwise =
      pure $ TimeZone 0 False "UTC"  -- TODO

sampleJSON :: IO String
sampleJSON = readFile "sample/metadata.json"

example :: IO (Either String AlphaMetaData)
example = do
  metadataString <- pack <$> sampleJSON
  let
    eamd :: Either String AlphaMetaData
    eamd = eitherDecode metadataString
  return eamd

sampleJSON2 :: IO String
sampleJSON2 = readFile "sample/metadata2.json"

example2 :: IO (Either String AlphaMetaData)
example2 = do
  metadataString <- pack <$> sampleJSON2
  let
    eamd :: Either String AlphaMetaData
    eamd = eitherDecode metadataString
  return eamd

fakeMetaDataEncode = do
  now <- getZonedTime
  let metaData = AlphaMetaData "foo" "X" (zonedTimeToLocalTime now) "1.0" "1.0" (zonedTimeZone now)
      metaDataEncoded = encode metaData
  putStrLn $ unpack metaDataEncoded

