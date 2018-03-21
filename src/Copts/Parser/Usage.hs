module Copts.Parser.Usage
    (Pattern (..), Usage, usage, exclusive)
    where


import Text.Megaparsec.Char (eol, char, string)
import Text.Megaparsec (Parsec, between, try, some, many, label)
import Prelude (Show, Eq, String, ($), (.), (++), concat, foldr1, map, init, last)
import Control.Applicative ((<*>), (*>), (<*), (<|>), liftA2, pure)
import Data.Functor ((<$>), (<$))
import Data.Void

import qualified Control.Applicative as A

import Copts.Applicative
import Copts.Parser.Combinators
import qualified Copts.Parser.Element as E


data Pattern = Optional Usage
             | Required Usage
             | Exclusive [Usage]
             | Repeated Pattern
             | A E.Element
             | Options
             deriving (Show, Eq)

type Usage = [Pattern]

type Parser = Parsec Void String


any = tryAll . map (spaces *>)

element = A <$> E.element
command = A <$> E.command
argument = A <$> E.argument


exclusive = label "exclusive group"
    $ Exclusive
    <$> patterns
    <:> (some $ separator '|' *> patterns)
    where patterns = some . try . any $ [repeated, element, required, optional]

repeated = label "repeated group"
    $ Repeated
    <$> pattern
    <* string "..."
    where pattern = any [argument, optional, required]

required = label "required group"
    $ Required
    <$> between (char '(') (char ')') (some pattern)
    where pattern = any [exclusive, repeated, element, optional]

optional = label "optional group"
    $ Optional
    <$> between (char '[') (char ']') (some pattern)
    where pattern = any [exclusive, repeated, element, required]

options = pure Options <* string "[options]"

usage = spaces
    *> command
    <:> (try shortcut <|> pure [])
    <++> many (any [exclusive, repeated, element, required, optional])
    where shortcut = pure <$> (spaces *> options <* spaces)
