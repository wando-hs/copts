module OptionParser (Option (..)
                   , Flag (..)
                   , Parameter (..)
                   , OptionParser.option) where

import qualified Text.Megaparsec.String (Parser)
import Data.Set (fromList, elemAt, size)
import Data.Maybe (isJust)
import Control.Applicative
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

parameter Nothing = pure Nothing
parameter (Just name) = Just
    <$> Parameter name
    <$> optional (try defaultValue)

name prefix = optional
    $ try
    $ ignoreOneOf prefix
    *> (some upperChar <|> delimitedBy "<" ">")

shortFlag = liftA2 (,) flag (name " ")
    where flag = ShortName
            <$ separator ','
            <* dash
            <*> letterChar

longFlag = liftA2 (,) flag (name "= ")
    where flag = LongName
            <$ separator ','
            <* dashes 2
            <*> some (alphaNumChar <|> dash)

validateNames names
    | size set == 1 = Right $ elemAt 0 set
    | null set      = Right Nothing
    | otherwise     = Left "Ta de brincation"
    where set = fromList $ filter isJust names

flags = do
    (fs, ns) <- internalize <*> some (try shortFlag <|> try longFlag)
    either fail (\n -> pure (fs, n)) (validateNames ns)

option = do
    (fs, name) <- flags
    desc <- description
    p <- parameter name
    pure $ Option fs p desc
