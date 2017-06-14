module Copts.Parser.Usage
    (Pattern (..), Usage, usage, exclusive)
    where

import Text.Megaparsec (char, between, try, some, many, string, eol, label)
import Prelude (Show, Eq, ($), (.), (++), concat, foldr1, map, init, last)
import Control.Applicative ((<*>), (*>), (<*), (<|>), liftA2, pure)
import Data.Functor ((<$>), (<$))

import Copts.Applicative
import Copts.Parser.Combinators
import qualified Copts.Parser.Element as E


data Pattern = Optional [Pattern]
	     | Required [Pattern]
             | Repeated Pattern
             | Exclusive [Pattern]
             | A E.Element
             deriving (Show, Eq)

type Usage = [Pattern]

any = tryAll . map (spaces *>)

element = A <$> E.element
command = A <$> E.command
argument = A <$> E.argument


exclusive = label "exclusive group"
	$ Exclusive . concat
	<$> components
	<:> (some $ separator '|' *> components)
    where components = some . try . any $ [repeated, element, required, optional]

repeated = label "repeated group"
	$ Repeated
	<$> component
	<* string "..."
    where component = any [argument, optional, required]

required = label "required group"
	$ Required
	<$> between (char '(') (char ')') (some component)
    where component = any [exclusive, repeated, element, optional]

optional = label "optional group"
	$ Optional
	<$> between (char '[') (char ']') (some component)
    where component = any [exclusive, repeated, element, required]

usage = command <:> many (any [exclusive, repeated, element, required, optional])
