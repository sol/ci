{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Travis.Syntax where

import           Data.Proxy
import           Data.Aeson.Types
import           GHC.Generics

import           GenericsUtil

data Matrix = Matrix {
  include :: [Include]
} deriving (Eq, Show, Generic, ToJSON)

data Include = Include {
  env :: String
, addons :: Addons
} deriving (Eq, Show, Generic, ToJSON)

data Addons = Addons {
  apt :: Apt
} deriving (Eq, Show, Generic, ToJSON)

data Apt = Apt {
  packages :: [String]
, sources :: [String]
} deriving (Eq, Show, Generic, ToJSON)

data Config = Config {
  sudo :: Bool
, language :: String
, matrix :: Matrix
, before_install :: [String]
, install :: [String]
, script :: [String]
} deriving (Eq, Show, Generic, ToJSON)

fields :: [String]
fields =
     selectors (Proxy :: Proxy Config)
  ++ selectors (Proxy :: Proxy Apt)
  ++ selectors (Proxy :: Proxy Include)
