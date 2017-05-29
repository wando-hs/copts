module OptionParser (Option (..)
                   , Flag (..)
                   , Parameter (..)
                   , OptionParser.option
                   , shortName) where

import Text.Megaparsec

import Combinators


type Description = String
type DefaultValue = String
type Name = String

data Flag = LongName String | ShortName Char
    deriving (Show, Eq)

data Parameter = Parameter Name (Maybe DefaultValue)
    deriving (Show, Eq)

data Option = Option [Flag] (Maybe Parameter) Description
    deriving (Show, Eq)

description = spaces 2
    *> space
    *> manyTill anyChar stop
      where stop = lookAhead (string "[default:" <|> eol)

defaultValue = space
    *> delimitedBy "[default: " "]"

name = try (ignore ' ' *> some upperChar)
    <|> (ignore '=' *> delimitedBy "<" ">")

parameter Nothing = pure Nothing
parameter (Just name) = Just
    <$> Parameter name
    <$> try' defaultValue

shortName = ShortName <$ space
    <* dashes 1
    <*> letterChar

longName = LongName
    <$ space
    <* dashes 2
    <*> some (alphaNumChar <|> dash)

option = do
    flags <- some $ try shortName <|> try longName
    pName <- try' name
    desc <- description
    p <- parameter pName
    pure $ Option flags p desc
