module Helpers (parse, failWith) where

import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec.String (Parser)
import qualified Text.Megaparsec as M (Dec (..), parse, errorCustom)

import Data.Set (fromList)
import Data.Bifunctor (first)

parse :: (Show a, Eq a) => (Parser a, String) -> a -> Expectation
parse (parser, text) result = M.parse parser "" text `shouldParse` result

failWith :: (Show a, Eq a) => Parser a -> String -> [String] -> Expectation
failWith p = (. err) . shouldBe . fail
    where err  = Left . fromList . fmap M.DecFail
          fail = first M.errorCustom . M.parse p ""
