{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Config where

import           Data.List
import           Data.Maybe
import           Data.Version
import           Data.Yaml
import           Data.ByteString (ByteString)

import qualified Config.Syntax as Syntax

parseConfig :: [Compiler] -> ByteString -> Either String Config
parseConfig compilers = fmap fromSyntax . decodeEither
  where
    fromSyntax :: Syntax.Config -> Config
    fromSyntax (Syntax.Config ghc travis) = Config (map (versionToCompiler compilers) ghc) travis

versionToCompiler :: [Compiler] -> Version -> Compiler
versionToCompiler compilers ghc = fromMaybe (Compiler ghc latestCabal) mCompiler
  where
    mCompiler = find ((== ghc) . compilerGhcVersion) compilers
    latestCabal = maximum (map compilerCabalVersion compilers)

data Config = Config {
  configCompilers :: [Compiler]
, configTravis :: Maybe Object
} deriving (Eq, Show)

data Compiler = Compiler {
  compilerGhcVersion :: Version
, compilerCabalVersion :: Version
} deriving (Eq, Show)
