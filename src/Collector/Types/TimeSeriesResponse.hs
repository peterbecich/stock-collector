{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module Collector.Types.TimeSeriesResponse where

import GHC.Generics

import Data.Aeson
import Data.Aeson.Types (Parser, parse, parseMaybe)

import Data.Map (Map, empty)
import Data.Time.LocalTime

import Collector.Types.AlphaMetaData
import Collector.Types.Tick (Tick')

data TimeSeriesResponse =
  TimeSeriesResponse { metaData :: AlphaMetaData
                     , ticks :: Map LocalTime Tick'
                     } deriving (Generic, Show)
