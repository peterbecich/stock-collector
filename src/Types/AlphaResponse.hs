{-# LANGUAGE DeriveGeneric #-}

module Types.AlphaResponse where

import GHC.Generics

import Types.Exchange
import Types.Stock
import Types.Tick

data AlphaResponse = AlphaResponse { exchange :: Exchange
                                   , stock :: Stock
                                   , ticks :: [Tick]
                                   } deriving (Generic, Show)
