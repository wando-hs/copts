module Copts.Parser.OptionDetails
    (OptionDetail (..), Parameter (..), details)
    where


import Text.Megaparsec (try, eol, char, eof, between, string, noneOf, lookAhead, anyChar, someTill)
import Control.Applicative (pure, optional, many, some, (<|>), (*>), (<*>))
import Data.Set (fromList, elemAt, size)
import Data.Either (Either (..), either)
import Data.Maybe (Maybe (..), isJust)
import Control.Monad (void, fail)
import Data.Functor ((<$>))

import Copts.Applicative
import Copts.Parser.Combinators
import Copts.Parser.Element


data Parameter = Parameter String (Maybe String)
    deriving (Show, Eq)

data OptionDetail = Details [Flag] (Maybe Parameter) String
    deriving (Show, Eq)


description = someTill anyChar (try eof <|> try parameter)
    where parameter = try spaces <* lookAhead (string "[default: ")
          eof = spaces *> lookAhead end

defaultValue = string "[default: "
    *> someTill anyChar (char ']' <* optional point)

parameter Nothing = pure Nothing
parameter (Just name) = Just . Parameter name
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
    spaces
    (fs, name) <- synonymous
    desc <- string "  " *> spaces *> description
    p <- parameter name
    pure $ Details fs p desc
