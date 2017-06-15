module Copts.Parser
    (
      Help (..)
    , Usage (..)
    , Pattern (..)
    , OptionDetail (..)
    , Parameter (..)
    , Flag (..)
    , Element (..)
    , help
    ) where


import Text.Megaparsec (string, anyChar, manyTill, space, newline, try)
import Control.Applicative ((*>), pure, some, many)
import Prelude (Show, Eq, String, null, ($))

import Copts.Applicative
import Copts.Parser.Usage
import Copts.Parser.Element
import Copts.Parser.Combinators
import Copts.Parser.OptionDetails


data Help = Simple String [Usage] | Complex String [Usage] [OptionDetail]
    deriving (Show, Eq)


description = manyTill anyChar $ try (space *> string "Usage:")

options = space *> details <:> many (try line)
    where line = space *> details

usages = space *> usage <:> many (try line)
    where line = newline *> spaces *> usage


help = do
    d <- description
    u <- usages
    space *> string "Options:"

    o <- options
    pure $ if null o then Simple d u else Complex d u o
