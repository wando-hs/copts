module OptionParser (Option (..)) where

import Control.Monad (void)

import Text.Megaparsec
import Text.Megaparsec.Expr
import Text.Megaparsec.String

data Option = ShortName Char deriving (Show)

dash = char '-'
ignoreMany = void . many . char
dashes n = void $ count n dash

shortName :: Parser Option
shortName = ShortName
    <$ ignoreMany ' '
    <* dashes 1
    <*> letterChar
