{-# LANGUAGE OverloadedStrings #-}


module Psql where

import           Prelude hiding (sum)

import Data.Functor ((<$>), fmap)

import           Opaleye (Column, Table(Table),
                           required, optional, (.==), (.<),
                           arrangeDeleteSql, arrangeInsertManySql,
                           arrangeUpdateSql, arrangeInsertManyReturningSql,
                           PGInt4, PGFloat8)

-- https://www.stackage.org/haddock/lts-9.10/opaleye-0.5.4.0/Opaleye-Manipulation.html#v:runInsertMany
import Opaleye.Manipulation

import Data.Int (Int64)
import           Data.Profunctor.Product (p6)
import           Data.Profunctor.Product.Default (def)
import qualified Opaleye.Internal.Unpackspec as U
import qualified Opaleye.Table as T
import qualified Opaleye.PGTypes as P
import qualified Opaleye.Constant as C
import Data.Time.LocalTime

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Internal (Connection)

import Data.Map (Map, empty, size, mapKeys, toList, assocs)

import qualified Data.Yaml.Config as Config


import Types.Tick

-- see TutorialManipulation.lhs

table :: String -> Table
      ((Column P.PGTimestamptz, Column P.PGFloat8, Column P.PGFloat8, Column P.PGFloat8, Column P.PGFloat8, Column P.PGInt4))
      ((Column P.PGTimestamptz, Column P.PGFloat8, Column P.PGFloat8, Column P.PGFloat8, Column P.PGFloat8, Column P.PGInt4))      
table tickerSymbol = T.Table tickerSymbol (p6 ( required "time"
                                            ,  required "open"
                                            , required "high"
                                            , required "low"
                                            , required "close"
                                            , required "volume" ))


insertTicks :: String -> Map ZonedTime Tick -> Connection -> IO Int64
insertTicks tickerSymbol ticks connection = let
  kvlist :: [(ZonedTime, Tick)]
  kvlist = assocs ticks
  pgticks :: [
    (Column P.PGTimestamptz
    , Column P.PGFloat8
    , Column P.PGFloat8
    , Column P.PGFloat8
    , Column P.PGFloat8
    , Column P.PGInt4)]
  pgticks = fmap (\(zt, tick) -> tickToPostgres zt tick) kvlist
    
  in runInsertMany connection (table tickerSymbol) pgticks

getPsqlConnection :: IO Connection
getPsqlConnection = do
  let
    path :: FilePath
    path = "conf/collector.yaml"
  config <- Config.load path
  db <- Config.subconfig "db" config
  postgres <- Config.subconfig "postgres" db
  dbname <- Config.lookup "dbname" postgres
  user <- Config.lookup "user" postgres
  password <- Config.lookup "password" postgres
  ip <- Config.lookup "ip" postgres
  port <- Config.lookup "port" postgres
  let
    connInfo :: ConnectInfo
    connInfo = ConnectInfo ip port user password dbname
  connect connInfo
  
