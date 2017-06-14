module Copts.Parser (Help (..), help) where

import Text.Megaparsec
import Text.Megaparsec.String

import Control.Monad (void)

import Copts.Applicative
import Copts.Parser.Usage
import Copts.Parser.OptionDetails

data Help = Simple String [[Usage]] | Complex String [Usage] [OptionDetail]
    deriving (Show, Eq)

session :: Parser a -> Parser [a]
session p = p <:> (many $ space *> p)

description = manyTill anyChar $ string "Usage:"

help = do
    d <- description
    u <- some $ try (space *> usage)
    -- _ <- space <* string "Options:"
    pure $ Simple d u
