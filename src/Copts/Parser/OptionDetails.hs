module Copts.Parser.OptionDetails
    (OptionDetail (..), Parameter (..), details)
    where


import Text.Megaparsec.Char (char, letterChar, anyChar, string)
import Text.Megaparsec (try, lookAhead, someTill)
import Control.Applicative (liftA2, pure, optional, many, (<|>), (*>), (<*>))
import Data.Set (fromList, elemAt, size)
import Data.Either (Either (..), either)
import Data.Maybe (Maybe (..), isJust)
import Control.Monad (fail)
import Data.Functor ((<$>))

import Copts.Applicative
import Copts.Parser.Data
import Copts.Parser.Combinators
import qualified Copts.Parser.Element as Element
import Copts.AST (Parameter(..))



data OptionDetail = Details [Element.Flag] (Maybe Parameter) String
    deriving (Show, Eq)


description = someTill anyChar (try eof <|> try param)
    where param = try spaces <* lookAhead (string "[default: ")
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


shortOption = liftA2 (,) flag (optional param)
    where flag = Element.Short <$ string "-" <*> letterChar
          param = try $ ignore ' ' *> Element.parameter

longOption = liftA2 (,) flag (optional param)
    where flag = Element.Long <$ string "--" <*> Element.name
          param = try $ ignoreOneOf ['=', ' '] *> Element.parameter

option' = try shortOption <|> longOption

options = option' <:> (many . try $ separator' *> option')
    where separator' = try (char ',' *> spaces) <|> spaces

synonymous = do
    (fs, ns) <- internalize <*> options
    either fail (\n -> pure (fs, n)) (validateNames ns)

details :: Parser OptionDetail
details = do
    spaces
    (fs, name) <- synonymous
    desc <- string "  " *> spaces *> description
    p <- parameter name
    pure $ Details fs p desc
