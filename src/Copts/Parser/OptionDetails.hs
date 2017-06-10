module Copts.Parser.OptionDetails
    (OptionDetail (..), Parameter (..), description, options, details)
    where

import Text.Megaparsec (try, eol, char, string, lookAhead, anyChar, manyTill)
import Data.Set (fromList, elemAt, size)
import Data.Maybe (isJust)
import Control.Applicative

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


description = manyTill anyChar stop
        where stop = lookAhead (string "[default:" <|> eol)

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
