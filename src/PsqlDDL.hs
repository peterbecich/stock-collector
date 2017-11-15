module PsqlDDL where

import Prelude

import Data.Monoid

import Control.Monad

-- https://stackoverflow.com/a/22548591/1007926
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import Psql (getTickerSymbols)

createTable :: String -> String
createTable tickerSymbol = "create table if not exists " <> tickerSymbol
       <> " (time timestamp not null primary key, open float8 not null, high float8 not null"
       <> ", low float8 not null, close float8 not null, volume int4 not null)"
       <> ";"

createTables :: [String] -> String
createTables tickerSymbols = foldr (\x y -> x ++ "   " ++ y) "" (createTable <$> tickerSymbols)

generateTables :: String -> IO ()
generateTables filename = do
  lsTickers <- getTickerSymbols filename
  putStrLn $ show lsTickers
  let tablesStr = createTables $ lsTickers
  writeFile "conf/tables.sql" tablesStr
  

