{-# LANGUAGE OverloadedStrings #-}

module Cass where

-- cassandra stuff

-- https://hackage.haskell.org/package/cql-io-0.16.0/docs/Database-CQL-IO.html

--import qualified Data.Text.Lazy as L (Text)
import qualified Data.Text.Lazy as Text (Text, pack)
import Data.Functor.Identity
import Database.CQL.IO as Client
import Database.CQL.Protocol
import qualified System.Logger as Logger


cassandraExample :: IO ()
cassandraExample = do
  -- https://hackage.haskell.org/package/tinylog-0.8/candidate/docs/System-Logger.html
  g <- Logger.new Logger.defSettings :: IO Logger.Logger
  c <- Client.init g defSettings
  let p = QueryParams One False () Nothing Nothing Nothing
      s :: QueryString S () ()
      s = "SELECT bar from stockmarket.foo"
      --q :: Client [Identity Text.Text]
      q = schema s p
  idtext <- runClient c q
  putStrLn $ show idtext
  shutdown c

-- http://cassandra.apache.org/doc/latest/cql/dml.html
-- https://hub.docker.com/_/cassandra/

createTableStatement :: String -> String
createTableStatement tickerSymbol =
  "CREATE TABLE IF NOT EXISTS stockmarket."++tickerSymbol++" (timestamp int, open float, high float, "
  ++ "low float, close float, volume int, PRIMARY KEY (timestamp));"
--  ++ " WITH CLUSTERING ORDER BY (timestamp ASC);"


clientCreateStockTable :: String -> Client.Client ()
clientCreateStockTable tickerSymbol =
  let
    p = QueryParams One False () Nothing Nothing Nothing
    tableStatement = createTableStatement tickerSymbol
    s = QueryString $ Text.pack tableStatement
  in Client.write s p


    
    

