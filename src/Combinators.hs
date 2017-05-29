module Combinators (dash, dashes, spaces, ignore, try', delimitedBy) where

import Control.Monad (void)

import Text.Megaparsec
import Text.Megaparsec.String


dash :: Parser Char
dash = char '-'

ignore :: Char -> Parser ()
ignore = void . try . char

ignoreOneOf :: String -> Parser ()
ignoreOneOf = void . try . oneOf

try' :: Parser a -> Parser (Maybe a)
try' combinator = try (Just <$> combinator) <|> pure Nothing

delimitedBy :: String -> String -> Parser String
delimitedBy a b = (string a) *> manyTill anyChar (string b)

spaces :: Int -> Parser ()
spaces n = void $ count n spaceChar

dashes :: Int -> Parser ()
dashes n = void $ count n dash
