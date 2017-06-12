module Copts.Parser.OptionDetails
    (OptionDetail (..), Parameter (..), details)
    where

import Text.Megaparsec.String (Parser)
import Text.Megaparsec (try, eol, char, eof, string, lookAhead, anyChar, someTill)
import Data.Set (fromList, elemAt, size)
import Data.Maybe (isJust)
import Control.Applicative
import Control.Monad (void)

import Copts.Applicative ((<:>))
import Copts.Parser.Combinators
import Copts.Parser.Element


type Description = String
type DefaultValue = String
type Name = String

data Parameter = Parameter Name (Maybe DefaultValue)
    deriving (Show, Eq)

data OptionDetail = Details [Flag] (Maybe Parameter) Description
    deriving (Show, Eq)


description = someTill anyChar (try end <|> parameter)
    where parameter = void . lookAhead . string $ "[default:"

defaultValue = delimitedBy "[default: " "]"

parameter Nothing = pure Nothing
parameter (Just name) = Just
    <$> Parameter name
    <$> optional (try defaultValue)

validateNames names
    | null set      = Right Nothing
    | size set == 1 = Right $ elemAt 0 set
    | otherwise     = Left "Ta de brincation"
    where set = fromList $ filter isJust names

options = option' <:> (many . try $ separator *> option')
    where separator = try (char ',' *> spaces) <|> spaces

synonymous = do
    (fs, ns) <- internalize <*> options
    either fail (\n -> pure (fs, n)) (validateNames ns)

details = do
    (fs, name) <- synonymous
    desc <- string "  " *> spaces *> description
    p <- parameter name
    pure $ Details fs p desc
