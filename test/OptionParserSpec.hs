module OptionParserSpec where

import Test.Hspec
import Test.Hspec.Megaparsec

import Text.Megaparsec
import Text.Megaparsec.Expr
import Text.Megaparsec.String

import OptionParser

spec :: Spec
spec =
  describe "parsing short option" $ do
    it "... a valid option" $ do
      parse shortName "" "-a" `shouldBe` (Right $ ShortName 'a')
    it "... a valid option prefixed with spaces" $ do
      parse shortName "" "   -a" `shouldBe` (Right $ ShortName 'a')
    it "... a valid option posfixed with spaces" $ do
      parse shortName "" "-a    " `shouldBe` (Right $ ShortName 'a')
    it "... an invalid option propagate error" $ do
      parse shortName "" `shouldFailOn` "- a"
    it "... an invalid option propagate error" $ do
      parse shortName "" `shouldFailOn` "-1"
    it "... an invalid option propagate error" $ do
      parse shortName "" `shouldFailOn` "a"
    it "... an invalid option propagate error" $ do
      parse shortName "" `shouldFailOn` "-"


