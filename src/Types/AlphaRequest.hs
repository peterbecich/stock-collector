{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Types.AlphaRequest where

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

newtype Interval = Interval String deriving Show

interval = Interval "1min"

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

exampleRequest :: IO Request
exampleRequest = do
  key <- getKey
  formatRequest "MSFT" (interval) Compact key

badRequest :: IO Request
badRequest = do
  key <- getKey
  formatRequest "ASHUDSADUNTAHUD" (interval) Compact key


exampleRequestSteel :: IO Request
exampleRequestSteel = do
  key <- getKey
  formatRequest "X" (interval) Full key
