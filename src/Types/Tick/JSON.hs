{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Types.Tick.JSON where

import Data.Aeson
import qualified Data.Aeson as Aeson

import Data.Aeson.Types (Parser, parse, parseMaybe)

import qualified Data.Map as Mp (Map, empty)

import Types.Tick

import Data.Time.LocalTime

instance FromJSON Tick where  -- TODO unsafe !!!
  parseJSON :: Aeson.Value -> Parser Tick
  parseJSON = undefined

  -- withObject "Tick" $ \tick -> do
    -- open <- read <$> tick .: "1. open"
    -- high <- read <$> tick .: "2. high"
    -- low <- read <$> tick .: "3. low"
    -- close <- read <$> tick .: "4. close"
    -- volume <- read <$> tick .: "5. volume"
    -- return $ Tick open high low close volume

-- exampleTick = Tick 1.0 2.0 3.0 4.0 1234

ticksParser :: Object -> Parser (Mp.Map LocalTime Tick)
ticksParser wholeObject = wholeObject .: "Time Series (1min)"


getSampleResponse :: IO String
getSampleResponse = do
  
