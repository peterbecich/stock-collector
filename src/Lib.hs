{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Lib where

import Control.Concurrent (threadDelay, forkIO)
import System.Random

import GHC.Generics
import Data.Aeson
import Data.Aeson.Types (Parser, parse, parseMaybe)

import Data.Time.Clock
import Data.Time.LocalTime

import Data.Map (Map, empty, size, mapKeys, toList, elems, insert, assocs)
import qualified Data.Map.Lazy as Map ((!)) 

import qualified Data.HashMap.Lazy as HMap ((!), keys)

import Control.Monad
import Data.Functor

import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString as BS

import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as LS

import Network.HTTP.Simple

import Types.Exchange
import Types.Stock

import Types.AlphaRequest
import Types.AlphaResponse
import Types.AlphaResponse.JSON

import Types.Exchange.Psql (nasdaq, insertExchange)
import Types.Stock.Psql (bogusStock, insertStock)
import Types.Tick.Psql (insertTicks)

import DB.Psql
-- instance Eq ZonedTime where
--   (==) zt1 zt2 = (zonedTimeToUTC zt1) == (zonedTimeToUTC zt2)
  
-- instance Ord ZonedTime where
--   compare zt1 zt2 = compare (zonedTimeToUTC zt1) (zonedTimeToUTC zt2)

-- retrieveAlphaResponse :: Exchange -> Stock -> Request -> IO AlphaResponse
retrieveAlphaResponse exchange stock requestURI = do
  responseFAlphaResponse <- httpJSON requestURI :: IO (Response (Exchange -> Stock -> AlphaResponse))
  let
    fAlphaResponse :: (Exchange -> Stock -> AlphaResponse)
    fAlphaResponse = getResponseBody responseFAlphaResponse

    alphaResponse = fAlphaResponse exchange stock

  return alphaResponse


retrieveSteel = do
  req <- exampleRequestSteel
  retrieveAlphaResponse nasdaq bogusStock req
  

-- US Steel
retrieveAndInsertSteel = do
  psqlConn <- getPsqlConnection "conf/collector.yaml"
  req <- exampleRequestSteel
  alphaResponse <- retrieveAlphaResponse nasdaq bogusStock req
  rowsInserted <- insertTicks (ticks alphaResponse) psqlConn
  return rowsInserted
  
