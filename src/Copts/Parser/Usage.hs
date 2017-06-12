module Copts.Parser.Usage (Usage (..), usage, xor) where

import Text.Megaparsec (char, between, try, some, many, string, eol, newline)
import Prelude (Show, Eq, ($), (.), (++), concat, foldr1, map, init, last)
import Control.Applicative ((<*>), (*>), (<*), (<|>), liftA2, pure)
import Data.Functor ((<$>), (<$))

import Copts.Applicative ((<:>))
import Copts.Parser.Combinators
import Copts.Parser.Element

import Text.Megaparsec.String (Parser)

data Usage =  Optional [Usage]
           | Required [Usage]
           | Ellipsis [Usage]
           | XOR [Usage]
           | A Element
    deriving (Show, Eq)


any :: [Parser p] -> Parser p
any = tryAll . map (spaces *>)

elements = A <$> element

arguments = A <$> argument


xor = XOR . concat <$> components <:> (some $ separator '|' *> components)
    where components = some . try . any $ [elements, required, optional]

ellipsis = Ellipsis <$> some component <* string "..."
    where component = any [arguments, optional, required]

required = Required <$> between (char '(') (char ')') (some component)
    where component = any [xor, ellipsis, elements, optional]

optional = Optional <$> between (char '[') (char ']') (some component)
    where component = any [xor, ellipsis, elements, required]

usage = some $ try $ any [xor, ellipsis, elements, required, optional]
