module Copts.Parser
    ( Help (..)
    , Usage
    , Pattern (..)
    , OptionDetail (..)
    , Parameter (..)
    , Flag (..)
    , Element (..)
    , help
    ) where


import Text.Megaparsec (manyTill, try)
import Text.Megaparsec.Char (string, anyChar, space, newline)
import Control.Applicative ((*>), (<*), (<$>), (<*>), optional, many)
import Data.Maybe (Maybe(..))
import Prelude (Show(..), Eq, String, ($))

import Copts.Applicative
import Copts.Parser.Data
import Copts.Parser.Usage
import Copts.Parser.Element
import Copts.Parser.Combinators
import Copts.Parser.OptionDetails


data Help = Simple String [Usage] | Complex String [Usage] [OptionDetail]
    deriving (Show, Eq)


header text = space *> string text *> spaces

body parser = (space *> parser <* spaces) <:> many (try line)
    where line = newline *> spaces *> parser <* spaces

description = manyTill anyChar (try $ header "Usage:")

from d u (Just o) = Complex d u o
from d u Nothing  = Simple d u

help :: Parser Help
help = from <$> description <*> body usage <*> optional
    (try $ header "Options:" *> body details)
