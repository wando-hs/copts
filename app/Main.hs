module Main where


import Text.Megaparsec (parseMaybe)
import System.Environment (getArgs)
import Algebra.Graph (edgeList)

import Copts.Graph (graph)
import Copts.Normalizer (normalize)
import Copts.Parser (help)

print' :: Show a => a -> IO ()
print' = putStrLn . show

options :: [String] -> IO ()
options ("parse":text:_) = print' $ parseMaybe help text
options ("normalize":text:_) = print' $ normalize <$> parseMaybe help text
options ("graph":text:args) = print' $ map (edgeList . graph) <$> normalize <$> parseMaybe help text

options x = mempty

main = options =<< getArgs
