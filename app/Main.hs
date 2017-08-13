module Main where


import Text.Megaparsec (parseMaybe, parse)
import System.Environment (getArgs)
import Data.Char (isUpper)
import Data.List (nub)
import Data.Maybe

import Copts.Graph
import Copts.Graph.Dot
import Copts.Normalizer
import Copts.Parser
import Copts.Predict

import Text.Show.Pretty


banana (Text _ _) = True
banana (Input _ label) = all isUpper label

ha (Text _ text) = text
ha (Input _ label) = label


options :: [String] -> IO ()
options ("parse":text:_) = pPrint $ parse help "" text

options ("normalize":text:_) = pPrint $ normalize <$> parseMaybe help text

options ("graph":text:_) = fromJust $ putStrLn . plot . snd . graph . normalize <$> parseMaybe help text

options ("predict":text:params) = fromJust $ putStrLn . unwords . nub . map ha . predictions <$> parseMaybe help text
    where predictions = filter banana . predict params . graph . normalize

options x = putStr . unlines $
    [ "Bash complete tool for POSIX help texts."
    , "Usage:"
    , "  copts (parse | normalize | plot | predict) <help>"
    , "  copts -h | --help"
    ]

main = options =<< getArgs
