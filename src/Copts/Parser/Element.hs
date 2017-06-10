module Copts.Parser.Element (Element (..)
              , Flag (..)
              , argument
              , command
              , option
              , option'
              , element
              ) where

import Text.Megaparsec hiding (option)
import Control.Applicative
import Copts.Applicative ((<:>))
import Copts.Parser.Combinators


data Flag = Long String | Short Char
    deriving (Show, Eq)

data Element = Command String | Argument String | Option (Flag, Maybe String)
    deriving (Show, Eq)


name = letterChar <:> (try $ many character)
    where character = try alphaNumChar <|> oneOf ['-', '_']

parameter = try argument' <|> some upperChar

shortOption = liftA2 (,) flag (optional $ param)
    where flag = Short <$ string "-" <*> letterChar
          param = try $ ignore ' ' *> parameter

longOption = liftA2 (,) flag (optional $ param)
    where flag = Long <$ string "--" <*> name
          param = try $ ignoreOneOf ['=', ' '] *> parameter

argument' = between (char '<') (char '>') name

command' = try name <|> string "--" <|> string "-"

option' = try shortOption <|> longOption


argument = Argument <$> argument'

command = Command <$> command'

option = Option <$> option'

element = tryAll [argument, option, command]
