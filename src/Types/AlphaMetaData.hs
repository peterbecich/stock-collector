{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module Types.AlphaMetaData where

import GHC.Generics


import Data.Time.LocalTime

data AlphaMetaData = AlphaMetaData { info :: String
                                   , symbol :: String
                                   , lastRefresh :: LocalTime
                                   , interval :: String
                                   , outputSize :: String
                                   , timeZone :: TimeZone
                                   } deriving (Generic, Show)


