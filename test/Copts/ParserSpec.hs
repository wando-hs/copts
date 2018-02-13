module Copts.ParserSpec where

import Helpers
import qualified TextSamples
import qualified ParsedSamples

import Test.Hspec

import Copts.Parser


spec =
  it "parse naval_fate example" $
    parse (help, TextSamples.navalFate) ParsedSamples.navalFate
