module Helpers where

import Test.Hspec
import Test.Hspec.Megaparsec

import qualified Text.Megaparsec as M

import Data.Void


type Parser = M.Parsec Void String


parse :: (Show a, Eq a) => (Parser a, String) -> a -> Expectation
parse (parser, text) result = M.parse parser "" text `shouldParse` result
