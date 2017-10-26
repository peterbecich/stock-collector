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


data Tick = Tick { --timestamp :: UTCTime
                   open :: Double
                 , high :: Double
                 , low :: Double
                 , close :: Double
                 , volume :: Int
                 } deriving (Generic, Show)

instance FromJSON Tick where  -- TODO unsafe !!!
  parseJSON :: Aeson.Value -> Parser Tick
  parseJSON = withObject "Tick" $ \tick -> do
    open <- read <$> tick .: "1. open"
    high <- read <$> tick .: "2. high"
    low <- read <$> tick .: "3. low"
    close <- read <$> tick .: "4. close"
    volume <- read <$> tick .: "5. volume"
    return $ Tick open high low close volume

-- getExampleTick = do
--   time <- getCurrentTime
--   return $ Tick time 1.0 2.0 3.0 4.0 1000

exampleTick = Tick 1.0 2.0 3.0 4.0 1234

ticksParser :: Object -> Parser (Mp.Map LocalTime Tick)
ticksParser wholeObject = wholeObject .: "Time Series (1min)"


-- clientInsertTick :: Tick -> Client.Client ()
-- clientInsertTick tick = let
--   p = QueryParams One False () Nothing Nothing Nothing
--   s' = show (open tick)++","++show (high tick)++","++show (low tick)++","++show (close tick)++","++show (volume tick)
--   s'' :: QueryString W () ()
--   s'' = QueryString $ Text.pack $ "INSERT INTO stockmarket.msft (timestamp, open, high, low, close, volume) values (12345,"
--     ++show (open tick)++","++show (high tick)++","
--     ++show (low tick)++","++show (close tick)++","
--     ++show (volume tick)++")"
--   -- s :: QueryString W () ()
--   -- s = "INSERT INTO stockmarket.msft (timestamp, open, high, low, close, volume) values (12345,1.0,1.0,1.0,1.0,100)"      
--   w = Client.write s'' p
--   in w
  

-- https://hackage.haskell.org/package/cql-io-0.16.0/docs/Database-CQL-IO.html#g:4
--storeTick :: Tick -> 

-- tickInsertExample :: Tick -> IO ()
-- tickInsertExample tick = do
--   g <- Logger.new Logger.defSettings :: IO Logger.Logger
--   c <- Client.init g Client.defSettings
--   let w = clientInsertTick tick
--   Client.runClient c w
--   putStrLn "done"
--   Client.shutdown c



