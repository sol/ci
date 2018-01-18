{-# LANGUAGE QuasiQuotes #-}
module RunSpec (spec) where

import           Test.Hspec
import           Data.String
import           Data.String.Interpolate.IsString
import           Data.String.Interpolate.Util

import           Run

spec :: Spec
spec = do
  describe "generateTravisConfig" $ do
    it "generates Travis config" $ do
      generateTravisConfig [i|
        ghc:
        - 7.10.3
        travis:
          script:
          - cabal configure --enable-tests --ghc-options=-Werror && cabal build && cabal test --show-details=direct
      |] `shouldBe` (Right . fromString . unindent) [i|
        sudo: false
        language: generic
        matrix:
          include:
          - compiler: GHC 7.10.3
            env: GHC_VERSION=7.10.3 CABAL_VERSION=1.22
            addons:
              apt:
                packages:
                - cabal-install-1.22
                - ghc-7.10.3
                sources:
                - hvr-ghc
        before_install: []
        install: []
        script:
        - cabal configure --enable-tests --ghc-options=-Werror && cabal build && cabal test
          --show-details=direct
      |]
