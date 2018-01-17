{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedLists #-}
module ConfigSpec (spec) where
import           Test.Hspec

import           Data.String.Interpolate.IsString

import           Config
import           Run

spec :: Spec
spec = do
  describe "parseConfig" $ do
    it "accepts ghc" $ do
      parseConfig compilers [i|
      ghc:
        - 7.6.3
        - 7.8.4
        - 7.10.3
      |] `shouldBe` Right Config {
        configCompilers = [
          Compiler [7,6,3] [1,18]
        , Compiler [7,8,4] [1,18]
        , Compiler [7,10,3] [1,22]
        ]
      , configTravis = Nothing
      }

  describe "versionToCompiler" $ do
    it "uses matching cabal version" $ do
      let
        ghc = [7,10,3]
        cabal = [1,22]
      versionToCompiler compilers ghc `shouldBe` Compiler ghc cabal

    context "with unknown version" $ do
      it "uses latest cabal version" $ do
        let
          ghc = [20,0,0]
          cabal = [2,0]
        versionToCompiler compilers ghc `shouldBe` Compiler ghc cabal
