module Copts.Parser.Combinators
  (end, spaces, separator, ignore, ignoreOneOf, internalize, tryAll, point)
  where


import Prelude (String, Char, last, init, foldr1, map, snd, fst, (.), ($), (++))
import Text.Megaparsec (try, label, oneOf, char, eol, eof)
import Control.Applicative (liftA2, optional, many, (<|>), (<*), pure)
import Text.Megaparsec.String (Parser)
import Control.Monad (void)
import Data.Functor (Functor, fmap)


end :: Parser ()
end = label "end of line or file"  parser
  where parser = void eol <|> void eof

ignore :: Char -> Parser ()
ignore = void . try . char

ignoreOneOf :: String -> Parser ()
ignoreOneOf = void . try . oneOf

point :: Parser Char
point = char '.'

spaces :: Parser ()
spaces = void . many . char $ ' '

separator :: Char -> Parser ()
separator c = spaces <* optional (ignore c) <* spaces

internalize :: Functor f => Parser (f (a, b) -> (f a, f b))
internalize = pure $ liftA2 (,) (fmap fst) (fmap snd)

tryAll :: [Parser a] -> Parser a
tryAll = foldr1 (<|>) . map try
