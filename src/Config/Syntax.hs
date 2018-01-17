{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Config.Syntax where

import           Data.Version
import           Data.Yaml
import           GHC.Generics

data Config = Config {
  ghc :: [Version]
, travis :: Maybe Object
} deriving (Eq, Show, Generic, FromJSON)
