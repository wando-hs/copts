module Copts.PredictSpec where

import Test.Hspec

import qualified NormalizedSamples

import Copts.Predict

navalFate args = predict NormalizedSamples.navalFate $ "naval_fate":args
myProgram args = predict NormalizedSamples.myProgram $ "my_program":args

spec = do
  it "predict naval_fate" $ do
    navalFate [] `shouldBe` ["naval_fate"]
    navalFate [""] `shouldBe` ["mine", "ship", "--version", "-h", "--help"]
    navalFate ["-"] `shouldBe` ["--version", "-h", "--help"]
    navalFate ["--"] `shouldBe` ["--version", "--help"]
    navalFate ["--v"] `shouldBe` ["--version"]
    navalFate ["s"] `shouldBe` ["ship"]
    navalFate ["ship", ""] `shouldBe` ["new", "shoot"]
    navalFate ["ship", "titanic", ""] `shouldBe` ["move"]
    navalFate ["ship", "titanic", "1", ""] `shouldBe` []
    navalFate ["mine", "set", "1", "2", ""] `shouldBe` ["--drifting", "--moored"]
    navalFate ["ship", "titanic", "move", "1", "2", ""] `shouldBe` ["--speed"]

  it "predict my_program" $
    myProgram [] `shouldBe`  ["my_program"]
