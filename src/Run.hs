{-# LANGUAGE OverloadedLists #-}
module Run where

import           Control.Exception
import qualified Data.ByteString as B

import           Config
import           Travis

die :: String -> IO a
die = throwIO . ErrorCall

main :: IO ()
main = B.readFile "ci.yaml" >>= either die (B.writeFile ".travis.yml") . generateTravisConfig

generateTravisConfig :: B.ByteString -> Either String B.ByteString
generateTravisConfig = fmap travisConfig . parseConfig compilers

compilers :: [Compiler]
compilers = [
    -- GHC 7.0.*
    Compiler [7,0,1] [1,16]
  , Compiler [7,0,2] [1,16]
  , Compiler [7,0,3] [1,16]
  , Compiler [7,0,4] [1,16]

    -- GHC 7.2.*
  , Compiler [7,2,1] [1,16]
  , Compiler [7,2,2] [1,16]

    -- GHC 7.4.*
  , Compiler [7,4,1] [1,16]
  , Compiler [7,4,2] [1,16]

    -- GHC 7.6.*
  , Compiler [7,6,1] [1,16]
  , Compiler [7,6,2] [1,16]
  , Compiler [7,6,3] [1,18]

    -- GHC 7.8.*
  , Compiler [7,8,1] [1,18]
  , Compiler [7,8,2] [1,18]
  , Compiler [7,8,3] [1,18]
  , Compiler [7,8,4] [1,18]

    -- GHC 7.10.*
  , Compiler [7,10,1] [1,22]
  , Compiler [7,10,2] [1,22]
  , Compiler [7,10,3] [1,22]

    -- GHC 8.0.*
  , Compiler [8,0,1] [1,24]
  , Compiler [8,0,2] [1,24]

    -- GHC 8.2.*
  , Compiler [8,2,1] [2,0]
  , Compiler [8,2,2] [2,0]

    -- GHC 8.4.*
  -- , Compiler [8,4,1] [2,0]
  ]
