module Main where

import Control.Monad (void)
import Control.Applicative (empty)
import Text.Megaparsec
import Text.Megaparsec.Expr
import Text.Megaparsec.String -- input stream is of type ‘String’
import qualified Text.Megaparsec.Lexer as L
import Lib

helpText =  "Naval Fate.\
\\n\
\Usage:\
\  naval_fate ship new <name>...\
\  naval_fate ship <name> move <x> <y> [--speed=<kn>]\
\  naval_fate ship shoot <x> <y>\
\  naval_fate mine (set|remove) <x> <y> [--moored|--drifting]\
\  naval_fate -h | --help\
\  naval_fate --version\
\\n\
\Options:\
\  -h --help     Show this screen.\
\  --version     Show version.\
\  -s --speed=<kn>  Speed in knots [default: 10].\
\  --moored      Moored (anchored) mine.\
\  --drifting    Drifting mine."

main :: IO ()
main = someFunc

data Option = LongName String
            | ShortName Char
            | LongNameWithParam String String
            | Description String
            | Default String
            deriving (Show)

dash = char '-'
greaterThan = char '>'
lessThan = char '<'

ignoreMany = void . many . char
ignore = void . try . char
dashes n = void $ count n dash

spaces :: Int -> Parser ()
spaces n = void $ count n $ char ' '

defStart = string "[default:"

param = ignore '='
    >> angles <|> some upperChar
    <?> "a non empty parameter name"
        where angles = between lessThan greaterThan (some $ noneOf "<>")

defaultValue = Default
    <$> between defStart (char ']') (some $ noneOf "]")

shortName = ShortName
    <$ ignoreMany ' '
    <* dashes 1
    <*> letterChar

longName' = withParam
    <$ ignoreMany ' '
    <* dashes 2
    <*> some (alphaNumChar <|> dash)
    <*> lookAhead anyChar
    where withParam name c = if (c == '=')
                                then LongNameWithParam name <$> param
                                else return $ LongName name

longName = longName' >>= id

description = Description
    <$ spaces 2
    <* ignoreMany ' '
    <*> manyTill anyChar stop
        where stop = lookAhead (defStart <|> eol)

z (LongNameWithParam _ _) = True
z _ = False

zuera :: Parser [Option]
zuera = do
    options <- some $ try shortName <|> try longName
    desc <- description
    if any z options
       then do
           def <- many $ try defaultValue
           return $ desc : (def ++ options)
       else return $ desc : options
