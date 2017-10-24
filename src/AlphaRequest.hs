{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module AlphaRequest where

import Prelude hiding (lookup)

import GHC.Generics
import Data.Aeson
import Data.Aeson.Types (Parser, parse, parseMaybe)

import Data.Time.Clock

import Network.HTTP.Simple (Request, parseRequest)

import Data.Yaml.Config

-- https://www.alphavantage.co/documentation/#intraday

type Symbol = [Char]

-- newtype Interval = Interval DiffTime
-- -- https://hackage.haskell.org/package/time-1.8.0.3/docs/Data-Time-Clock.html#t:DiffTime

-- intervals :: [Interval]
-- intervals =
--   [ Interval $ secondsToDiffTime 60
--   , Interval $ secondsToDiffTime 300
--   , Interval $ secondsToDiffTime 900
--   , Interval $ secondsToDiffTime 1800
--   , Interval $ secondsToDiffTime 3600
--   ]

newtype Interval = Interval String deriving Show

intervals :: [Interval]
intervals  = [Interval "1min", Interval "5min", Interval "15min", Interval "30min", Interval "60min"]

data OutputSize = Compact | Full

instance Show OutputSize where
  show Compact = "compact"
  show Full = "full"
  
newtype APIKey = APIKey String

formatRequest :: Symbol -> Interval -> OutputSize -> APIKey -> IO Request
formatRequest symbol (Interval mins) outputSize (APIKey apiKey) = do
  let requestString = "https://www.alphavantage.co/query?function=TIME_SERIES_INTRADAY&symbol="
        ++symbol++"&interval="++mins++"&outputsize="++(show outputSize)++"&apikey="++apiKey
  parseRequest requestString


-- msft15 :: Request
-- msft15 = "https://www.alphavantage.co/query?function=TIME_SERIES_INTRADAY&symbol=MSFT&interval=1min&outputsize=full&apikey="

-- https://hackage.haskell.org/package/yaml-config-0.4.0/docs/Data-Yaml-Config.html
getKey :: IO APIKey
getKey = do
  let
    path :: FilePath
    path = "conf/collector.yaml"
  config <- load path
  keys <- subconfig "keys" config
  alphaKey <- lookup "alphaVantage" keys
  return $ APIKey alphaKey

exampleRequest = do
  key <- getKey
  formatRequest "MSFT" (intervals !! 0) Compact key
