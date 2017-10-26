{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module Types.TimeSeriesResponse where

import GHC.Generics

import Data.Aeson
import Data.Aeson.Types (Parser, parse, parseMaybe)

import qualified Data.Map as Mp (Map, empty, toList)
import Data.Time.LocalTime
import Data.Time (UTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)

import Types.AlphaMetaData
import Types.Tick
import Cass (clientCreateStockTable)

import Data.Text (Text)
import Data.Functor.Identity

import qualified Database.CQL.IO as Client

import Database.CQL.Protocol
import qualified System.Logger as Logger

import qualified Data.Text.Lazy as Text (pack)

data TimeSeriesResponse =
  TimeSeriesResponse { metaData :: AlphaMetaData
                     , ticks :: Mp.Map ZonedTime Tick
                     } deriving (Generic, Show)

clientInsertTick :: String -> ZonedTime -> Tick -> Client.Client ()
clientInsertTick tickerSymbol zonedTime tick = let
  utc :: UTCTime
  utc = zonedTimeToUTC zonedTime
  epochSeconds :: Integer
  epochSeconds = floor (utcTimeToPOSIXSeconds utc)
  p = QueryParams One False () Nothing Nothing Nothing
  s' :: String
  s' = show (open tick)++","++show (high tick)++","
    ++show (low tick)++","++show (close tick)
    ++","++show (volume tick)
  s'' :: QueryString W () ()
  s'' = QueryString $ Text.pack
    $ "INSERT INTO stockmarket."
    ++ tickerSymbol
    ++ " (timestamp, open, high, low, close, volume) values ("
    ++ show (epochSeconds) ++ ","
    ++ show (open tick) ++ ","
    ++ show (high tick) ++ ","
    ++ show (low tick) ++ ","
    ++ show (close tick) ++ ","
    ++ show (volume tick) ++ ")"
  w = Client.write s'' p
  in w

clientInsertTicks :: TimeSeriesResponse -> Client.Client ()
clientInsertTicks tsr = do
  let
    tickerSymbol = symbol (metaData tsr)
    tickList ::[(ZonedTime, Tick)]
    tickList = Mp.toList (ticks tsr)
    clients :: [Client.Client ()]
    clients = fmap (
      \(zonedTime, tick) ->
        clientInsertTick tickerSymbol zonedTime tick
      ) tickList
  mergedClients <- sequence_ clients
  return mergedClients

insertTick :: String -> ZonedTime -> Tick -> IO ()
insertTick tickerSymbol zonedTime tick = do
  g <- Logger.new Logger.defSettings :: IO Logger.Logger
  c <- Client.init g Client.defSettings
  let w = clientInsertTick tickerSymbol zonedTime tick
  Client.runClient c w
  putStrLn "done"
  Client.shutdown c

insertTicks :: TimeSeriesResponse -> IO ()
insertTicks tsr = do
  g <- Logger.new Logger.defSettings :: IO Logger.Logger
  c <- Client.init g Client.defSettings
  let
      tickerSymbol = symbol (metaData tsr)
      makeTable = clientCreateStockTable tickerSymbol
  Client.runClient c makeTable
  putStrLn "made table"
  let
      w = clientInsertTicks tsr
  Client.runClient c w
  putStrLn "done"
  Client.shutdown c
  
