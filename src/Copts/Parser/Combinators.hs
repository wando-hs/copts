module Copts.Parser.Combinators
  (end, spaces, separator, ignore, ignoreOneOf, internalize, tryAll, delimitedBy)
  where


import Control.Monad (void)
import Control.Applicative (liftA2)

import Text.Megaparsec
import Text.Megaparsec.String

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
