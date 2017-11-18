{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Types.Tick.JSON where

import Data.Aeson
import qualified Data.Aeson as Aeson

import Data.Aeson.Types (Parser, parse, parseMaybe)

import qualified Data.Map as Mp (Map, empty, keys)
import Data.Text.Internal (Text)
import Data.ByteString.Lazy.Char8 (pack, unpack)

import Control.Monad

import Types.Stock
import Types.Stock.Psql (bogusStock)
import Types.Tick

import Data.Time.Clock
import Data.Time.LocalTime

instance FromJSON (UTCTime -> Stock -> Tick) where
  parseJSON = withObject "Tick" $ \tick -> do
    open <- read <$> tick .: "1. open"  -- read prone to runtime error
    high <- read <$> tick .: "2. high"
    low <- read <$> tick .: "3. low"
    close <- read <$> tick .: "4. close"
    volume <- read <$> tick .: "5. volume"
    return $ (\timestamp stock -> Tick timestamp open high low close volume stock)

-- ticksParser :: Object -> Parser (Mp.Map LocalTime (UTCTime -> Stock -> Tick))
-- ticksParser wholeObject = wholeObject .: "Time Series (1min)"

-- instance FromJSON (Mp.Map LocalTime (UTCTime -> Stock -> Tick))

exampleTickStr2 :: IO String
exampleTickStr2 = readFile "sample/tick2.json"

exampleDecodeTick :: IO (Either String Tick)
exampleDecodeTick = do
  tickString <- pack <$> exampleTickStr2
  now <- getCurrentTime
  let
    eFTick :: Either String (UTCTime -> Stock -> Tick)
    eFTick = eitherDecode tickString

  eTick <- pure $ do
    fTick <- eFTick
    return $ fTick now bogusStock

  return eTick
  

exampleTicksStr :: IO String
exampleTicksStr = readFile "sample/ticks.json"

--exampleDecodeTicks :: IO (Either String (Mp.Map LocalTime (UTCTime -> Stock -> Tick)))
exampleDecodeTicks = do
  ticksString <- pack <$> exampleTicksStr
  let
    eMFTicks :: Either String (Mp.Map LocalTime (UTCTime -> Stock -> Tick))
    eMFTicks = eitherDecode ticksString

    -- mFTicks :: Mp.Map LocalTime (UTCTime -> Stock -> Tick)
    -- (Right mFTicks) = eMFTicks
  putStrLn $ show $ Mp.keys <$> eMFTicks
  -- return eMFTicks
