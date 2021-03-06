module Copts.Parser.Element
    (Element (..), Flag (..), parameter, name, argument, command, option, element)
    where


import Text.Megaparsec.Char (string, oneOf, letterChar, char, upperChar, alphaNumChar)
import Text.Megaparsec (label, try, between)
import Control.Applicative (many, some, optional, liftA2, (<*>), (*>), (<|>))
import Prelude (Show, Eq, String, ($))
import Data.Functor ((<$>), (<$))
import Data.Maybe (Maybe)

import Copts.Applicative
import Copts.Parser.Data
import Copts.Parser.Combinators
import Copts.AST (Flag(..))


data Element = Command String | Argument String | Option (Flag, Maybe String)
    deriving (Show, Eq)


name :: Parser String
name = letterChar <:> try (many character)
    where character = try alphaNumChar <|> oneOf ['-', '_']

argument' = between (char '<') (char '>') name

parameter :: Parser String
parameter = label "parameter" $ try argument' <|> some upperChar

shortOption = liftA2 (,) flag (optional param)
    where flag = Short <$ string "-" <*> letterChar
          param = try $ ignore ' ' *> parameter

longOption = liftA2 (,) flag (optional param)
    where flag = Long <$ string "--" <*> name
          param = try $ ignore '=' *> parameter

option' = try shortOption <|> longOption

command' = try name <|> string "--" <|> string "-"


argument :: Parser Element
argument = label "argument" $ Argument <$> argument'

command :: Parser Element
command = label "command" $ Command <$> command'

option :: Parser Element
option = label "option" $ Option <$> option'

element :: Parser Element
element = tryAll [argument, option, command]
