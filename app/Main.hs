module Main where


import Text.Megaparsec (parseMaybe, parse)
import System.Environment (getArgs)
import Data.Maybe

import Copts.Graph (graph')
import Copts.Graph.Dot
import Copts.Normalizer (normalize)
import Copts.Parser (help)


options :: [String] -> IO ()
options ("parse":text:_) = print $ parse help "" text
options ("normalize":text:_) = print $ normalize <$> parseMaybe help text
options ("graph":text:_) = fromJust $ putStrLn . plot . graph' . normalize <$> parseMaybe help text
options x = mempty

main = options =<< getArgs
