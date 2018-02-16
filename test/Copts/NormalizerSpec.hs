module Copts.NormalizerSpec where

import Test.Hspec

import qualified NormalizedSamples
import qualified ParsedSamples

import Copts.Normalizer

spec = do
  it "normalize naval_fate example" $
    normalize ParsedSamples.navalFate `shouldBe` NormalizedSamples.navalFate

  it "normalize myProgram example" $
    normalize ParsedSamples.myProgram `shouldBe` NormalizedSamples.myProgram
