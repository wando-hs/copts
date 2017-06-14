module Copts.Parser.Combinators
  (end, spaces, separator, ignore, ignoreOneOf, internalize, tryAll, delimitedBy)
  where


import Prelude (String, Char, last, init, foldr1, map, snd, fst, (.), ($), (++))
import Text.Megaparsec (try, oneOf, char, anyChar, string, manyTill, eol, eof)
import Control.Applicative (liftA2, optional, many, (<|>), (<*), (*>), pure)
import Text.Megaparsec.String (Parser)
import Control.Monad (void)
import Data.Functor (Functor, fmap)


end :: Parser ()
end = (try $ void eol) <|> void eof

ignore :: Char -> Parser ()
ignore = void . try . char

ignoreOneOf :: String -> Parser ()
ignoreOneOf = void . try . oneOf

delimitedBy :: String -> String -> Parser String
delimitedBy a b = (string a) *> manyTill anyChar (string b)

spaces :: Parser ()
spaces = void . many . char $ ' '

separator :: Char -> Parser ()
separator c = spaces <* optional (ignore c) <* spaces

internalize :: Functor f => Parser (f (a, b) -> (f a, f b))
internalize = pure $ liftA2 (,) (fmap fst) (fmap snd)

tryAll :: [Parser a] -> Parser a
tryAll l = foldr1 (<|>) (map try (init l) ++ [last l])
