module Main where


import Text.Megaparsec (parse, parseMaybe)
import System.Environment (getArgs)

import Copts.Normalizer (normalize)
import Copts.Parser (help)


options :: [String] -> IO ()
options ("parse":text:_) = putStrLn . show $ parseMaybe help text
options ("normalize":text:_) = putStrLn . show $ normalize <$> parseMaybe help text
options x = mempty

main = options =<< getArgs
