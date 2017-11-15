{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
module Types.Tick where

import GHC.Generics

import Data.Aeson
import qualified Data.Aeson as Aeson

import Data.Aeson.Types (Parser, parse, parseMaybe)

import qualified Data.Map as Mp (Map, empty)

import Data.Time.LocalTime
import Data.Time (UTCTime, getCurrentTime)

import Data.Text (Text)
import Data.Functor.Identity

import qualified Database.CQL.IO as Client

import Database.CQL.Protocol
import qualified System.Logger as Logger

import qualified Data.Text.Lazy as Text (pack)

import           Opaleye (Column)
import qualified Opaleye.PGTypes as P


data Tick = Tick { --timestamp :: UTCTime
                   open :: Double
                 , high :: Double
                 , low :: Double
                 , close :: Double
                 , volume :: Int
                 } deriving (Generic, Show)

-- for Opaleye
tickToTuple :: Tick -> (Double, Double, Double, Double, Int)
tickToTuple (Tick open high low close volume) = (open, high, low, close, volume)

tickToPostgres :: ZonedTime -> Tick -> (Column P.PGTimestamptz, Column P.PGFloat8, Column P.PGFloat8, Column P.PGFloat8, Column P.PGFloat8, Column P.PGInt4)
tickToPostgres zonedTime (Tick open high low close volume) = let
  utc = zonedTimeToUTC zonedTime
  pgutc = P.pgUTCTime utc
  in (pgutc, P.pgDouble open, P.pgDouble high, P.pgDouble low, P.pgDouble close, P.pgInt4 volume)

instance FromJSON Tick where  -- TODO unsafe !!!
  parseJSON :: Aeson.Value -> Parser Tick
  parseJSON = withObject "Tick" $ \tick -> do
    open <- read <$> tick .: "1. open"
    high <- read <$> tick .: "2. high"
    low <- read <$> tick .: "3. low"
    close <- read <$> tick .: "4. close"
    volume <- read <$> tick .: "5. volume"
    return $ Tick open high low close volume

exampleTick = Tick 1.0 2.0 3.0 4.0 1234

ticksParser :: Object -> Parser (Mp.Map LocalTime Tick)
ticksParser wholeObject = wholeObject .: "Time Series (1min)"

