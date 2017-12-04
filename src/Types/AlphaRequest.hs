{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Types.AlphaRequest where

import Prelude hiding (lookup)

import Data.Aeson
import Data.Aeson.Types (Parser, parse, parseMaybe)
import Data.Time.Clock
import Data.Yaml.Config
import GHC.Generics
import Network.HTTP.Simple (Request, parseRequest)
import System.Environment

import Types.Stock

-- https://www.alphavantage.co/documentation/#intraday

type Symbol = [Char]

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
-- getKey :: IO APIKey
-- getKey = do
--   let
--     path :: FilePath
--     path = "conf/collector.yaml"
--   config <- load path
--   keys <- subconfig "keys" config
--   alphaKey <- lookup "alphaVantage" keys
--   return $ APIKey alphaKey

-- https://hackage.haskell.org/package/base-4.10.0.0/docs/System-Environment.html
getKey :: IO APIKey
getKey = APIKey <$> getEnv "ALPHA_VANTAGE_KEY"
  

simpleCompactRequest :: Stock -> IO Request
simpleCompactRequest stock = do
  key <- getKey
  formatRequest (symbol stock) interval Compact key

simpleFullRequest :: Stock -> IO Request
simpleFullRequest stock = do
  key <- getKey
  formatRequest (symbol stock) interval Full key

