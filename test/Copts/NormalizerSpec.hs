module Copts.NormalizerSpec where

import Test.Hspec

import qualified NormalizedSamples
import qualified ParsedSamples

import Copts.Normalizer

spec =
  it "normalize naval_fate example" $
    normalize ParsedSamples.navalFate `shouldBe` NormalizedSamples.navalFate
