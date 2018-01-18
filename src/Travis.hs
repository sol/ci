{-# LANGUAGE ViewPatterns #-}
module Travis where

import           Data.Version
import           Data.Ord
import           Data.Monoid
import           Data.Aeson.Types
import qualified Data.Yaml.Pretty as Yaml
import           Data.ByteString (ByteString)
import           Data.Text (Text)
import qualified Data.Text as T

import qualified Config as CI
import           Travis.Syntax as Syntax

travisConfig :: CI.Config -> ByteString
travisConfig (CI.Config compilers travis) = encode travis (mkConfig compilers)

encode :: Maybe Object -> Config -> ByteString
encode addFields value = Yaml.encodePretty config $ case toJSON value of
  Object o -> Object (maybe o (<> o) addFields)
  v -> v
  where
    config :: Yaml.Config
    config = Yaml.setConfCompare (comparing key) Yaml.defConfig

    key :: Text -> (Maybe Int, Text)
    key name = (lookup name keys, name)

    keys :: [(Text, Int)]
    keys = zip (map T.pack Syntax.fields) [1..]

mkConfig :: [CI.Compiler] -> Config
mkConfig compilers = defaultConfig {
    matrix = mkMatrix compilers
  }

defaultConfig :: Config
defaultConfig = Config {
    sudo = False
  , language = "generic"
  , matrix = Matrix []
  , before_install = []
  , install = []
  , script = []
  }

mkMatrix :: [CI.Compiler] -> Matrix
mkMatrix = Matrix . map mkInclude

mkInclude :: CI.Compiler -> Include
mkInclude (CI.Compiler (showVersion -> ghc) (showVersion -> cabal)) = Include {
    compiler = "GHC " ++ ghc
  , env = "GHC_VERSION=" ++ ghc ++ " CABAL_VERSION=" ++ cabal
  , addons = Addons {
      apt = Apt {
        packages = [
          "cabal-install-" ++ cabal
        , "ghc-" ++ ghc
        ]
      , sources = ["hvr-ghc"]
      }
    }
  }
