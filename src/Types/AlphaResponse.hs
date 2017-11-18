{-# LANGUAGE DeriveGeneric #-}

module Types.AlphaResponse where

import Data.List

import GHC.Generics

import Types.Exchange
import Types.Stock
import Types.Tick

data AlphaResponse = AlphaResponse { exchange :: Exchange
                                   , stock :: Stock
                                   , ticks :: [Tick]
                                   } deriving (Generic, Show)

-- TODO make safe with Maybe
-- unsafe!!
getLastTick :: AlphaResponse -> Tick
getLastTick (AlphaResponse _ _ ticks) = last ticks
