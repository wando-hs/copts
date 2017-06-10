module Copts.Parser.Usage (Usage (..), Type (..), usage) where

import Text.Megaparsec (char, between, try, some, many, string, eol, newline)
import Prelude (Show, Eq, ($), (.), (++), concat, foldr1, map, init, last)
import Control.Applicative ((<*>), (*>), (<*), (<|>), liftA2, pure)
import Data.Functor ((<$>), (<$))

import Copts.Parser.Combinators
import Copts.Parser.Element


data Type = Optional | Ellipsis | XOR | Required
    deriving (Show, Eq)

data Usage =  Groups Type [Usage] | Elements [Element]
    deriving (Show, Eq)


any = some . tryAll . map (spaces *>)

elements = Elements <$> (some . try $ spaces *> element)

arguments = Elements <$> (some . try $ spaces *> argument)


xor = Groups XOR . concat <$> components <:> (some $ separator *> components)
    where separator = spaces <* char '|' *> spaces
          components = any [elements, required, optional]

ellipsis = Groups Ellipsis <$> components <* string "..."
    where components = any [arguments, optional, required]

required = Groups Required <$> between (char '(') (char ')') components
    where components = any [xor, ellipsis, elements, optional]

optional = Groups Optional <$> between (char '[') (char ']') components
    where components = any [xor, ellipsis, elements, required]

usage = any [xor, ellipsis, elements, required, try optional]
