module Copts.Parser
    ( Help (..)
    , Usage (..)
    , Pattern (..)
    , OptionDetail (..)
    , Parameter (..)
    , Flag (..)
    , Element (..)
    , help
    ) where


import Text.Megaparsec (string, anyChar, manyTill, space, newline, try)
import Text.Megaparsec.String
import Control.Applicative ((*>), (<$>), (<*>), optional, pure, some, many)
import Data.Maybe (Maybe(..))
import Prelude (Show(..), Eq, String, null, map, unlines, (++), ($), (.))

import Copts.Applicative
import Copts.Parser.Usage
import Copts.Parser.Element
import Copts.Parser.Combinators
import Copts.Parser.OptionDetails


data Help = Simple String [Usage] | Complex String [Usage] [OptionDetail]
    deriving (Show, Eq)


header text = space *> string text *> spaces

description = manyTill anyChar (try $ header "Usage:")

usages = space *> usage <:> many (try line)
    where line = newline *> spaces *> usage

options = space *> details <:> many (try line)
    where line = space *> details

from d u (Just o) = Complex d u o
from d u Nothing = Simple d u

help = from
    <$> description
    <*> usages
    <*> optional (header "Options:" *> options)
