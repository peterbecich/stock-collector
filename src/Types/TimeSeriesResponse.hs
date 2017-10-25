{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module Types.TimeSeriesResponse where

import GHC.Generics

import Data.Aeson
import Data.Aeson.Types (Parser, parse, parseMaybe)

import qualified Data.Map as Mp (Map, empty)
import Data.Time.LocalTime
import Data.Time (UTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)

import Types.AlphaMetaData
import Types.Tick

import Data.Text (Text)
import Data.Functor.Identity

import qualified Database.CQL.IO as Client

import Database.CQL.Protocol
import qualified System.Logger as Logger

import qualified Data.Text.Lazy as Text (pack)


-- data TimeSeriesResponse =
--   TimeSeriesResponse { metaData :: AlphaMetaData
--                      , ticks :: Mp.Map LocalTime Tick
--                      } deriving (Generic, Show)

data TimeSeriesResponse =
  TimeSeriesResponse { metaData :: AlphaMetaData
                     , ticks :: Mp.Map ZonedTime Tick
                     } deriving (Generic, Show)


clientInsertTick :: UTCTime -> Tick -> Client.Client ()
clientInsertTick timestamp tick = let
  p = QueryParams One False () Nothing Nothing Nothing
  s' = show (open tick)++","++show (high tick)++","++show (low tick)++","++show (close tick)++","++show (volume tick)
  s'' :: QueryString W () ()
  s'' = QueryString $ Text.pack
    $ "INSERT INTO stockmarket.msft (timestamp, open, high, low, close, volume) values ("
    ++ undefined
    ++ show (open tick) ++ "," ++ show (high tick) ++ ","
    ++ show (low tick) ++ "," ++ show (close tick) ++ ","
    ++ show (volume tick) ++ ")"
  -- s :: QueryString W () ()
  -- s = "INSERT INTO stockmarket.msft (timestamp, open, high, low, close, volume) values (12345,1.0,1.0,1.0,1.0,100)"      
  w = Client.write s'' p
  in w

-- clientInsertTicks :: TimeSeriesResponse -> Client.Client ()
-- clientInsertTicks tsr 


now = do
  utcNow <- getCurrentTime
  putStrLn $ show utcNow
  
