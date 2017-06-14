module Copts.Parser.Element
    (Element (..), Flag (..), argument, command, option, option', element)
    where


import Text.Megaparsec (string, label, try, char, letterChar, upperChar, oneOf,
                        between, alphaNumChar)
import Control.Applicative (many, some, optional, liftA2, (<*>), (*>), (<|>))
import Prelude (Show, Eq, String, Char, ($))
import Data.Functor ((<$>), (<$))
import Data.Maybe (Maybe)

import Copts.Applicative
import Copts.Parser.Combinators


data Flag = Long String | Short Char
    deriving (Show, Eq)

data Element = Command String | Argument String | Option (Flag, Maybe String)
    deriving (Show, Eq)


name = letterChar <:> (try $ many character)
    where character = try alphaNumChar <|> oneOf ['-', '_']

argument' = between (char '<') (char '>') name

parameter = label "parameter" $ try argument' <|> some upperChar

shortOption = liftA2 (,) flag (optional $ param)
    where flag = Short <$ string "-" <*> letterChar
          param = try $ ignore ' ' *> parameter

longOption = liftA2 (,) flag (optional $ param)
    where flag = Long <$ string "--" <*> name
          param = try $ ignoreOneOf ['=', ' '] *> parameter

command' = try name <|> string "--" <|> string "-"

option' = try shortOption <|> longOption


argument = label "argument" $ Argument <$> argument'

command = label "command" $ Command <$> command'

option = label "option" $ Option <$> option'

element = tryAll [argument, option, command]
