{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module Types.Tick where

import GHC.Generics

import Data.Aeson
import Data.Aeson.Types (Parser, parse, parseMaybe)

import Data.Map (Map, empty)

import Data.Time.LocalTime

data Tick = Tick { open :: Double
                 , high :: Double
                 , low :: Double
                 , close :: Double
                 , volume :: Int
                 } deriving (Generic, Show)

instance FromJSON Tick where  -- unsafe !!!
  parseJSON = withObject "Tick" $ \tick -> do
    open <- read <$> tick .: "1. open"
    high <- read <$> tick .: "2. high"
    low <- read <$> tick .: "3. low"
    close <- read <$> tick .: "4. close"
    volume <- read <$> tick .: "5. volume"
    return $ Tick open high low close volume

ticksParser :: Object -> Parser (Map LocalTime Tick)
ticksParser wholeObject = wholeObject .: "Time Series (1min)"
