{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module Types.TimeSeriesResponse where

import GHC.Generics

import Data.Aeson
import Data.Aeson.Types (Parser, parse, parseMaybe)

import Data.Map (Map, empty)
import Data.Time.LocalTime

import Types.AlphaMetaData
import Types.Tick (Tick)

data TimeSeriesResponse =
  TimeSeriesResponse { metaData :: AlphaMetaData
                     , ticks :: Map LocalTime Tick
                     } deriving (Generic, Show)
