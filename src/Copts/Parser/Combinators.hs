module Copts.Parser.Combinators (dash
  , dashes
  , spaces
  , ignore
  , ignoreOneOf
  , separator
  , internalize
  , tryAll
  , delimitedBy) where

import Control.Monad (void)
import Control.Applicative (liftA2)

import Text.Megaparsec
import Text.Megaparsec.String


dash :: Parser Char
dash = char '-'

ignore :: Char -> Parser ()
ignore = void . try . char

ignoreOneOf :: String -> Parser ()
ignoreOneOf = void . try . oneOf

delimitedBy :: String -> String -> Parser String
delimitedBy a b = (string a) *> manyTill anyChar (string b)

spaces :: Parser ()
spaces = void . many . char $ ' '

dashes :: Int -> Parser ()
dashes n = void $ count n dash

separator :: Char -> Parser ()
separator c = space <* optional (ignore c) <* space

internalize :: Functor f => Parser (f (a, b) -> (f a, f b))
internalize = pure $ liftA2 (,) (fmap fst) (fmap snd)

tryAll :: [Parser a] -> Parser a
tryAll l = foldr1 (<|>) (map try (init l) ++ [last l])
