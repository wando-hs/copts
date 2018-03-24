module Main (main) where


import Text.Megaparsec (parse)
import System.Environment (getArgs)
import System.IO (stderr, hPutStrLn)

import Copts.Graph
import Copts.Graph.Dot
import Copts.Normalizer
import Copts.Parser
import Copts.Predict

import Text.Show.Pretty


print' (Right x) = putStrLn x
print' (Left x) = hPutStrLn stderr $ ppShow x


options :: [String] -> IO ()
options ("mirror":_:params) = pPrint params

options ("parse":text:_) = pPrint $ parse help "" text

options ("normalize":text:_) = pPrint $ normalize <$> parse help "" text

options ("graph":text:_) = print'
    $ plot . snd . graph . normalize
    <$> parse help "" text

options ("predict":text:params) = print'
    $ unwords . concatMap (flip predict params) . normalize
    <$> parse help "" text

options _ = putStr . unlines $
    [ "Bash complete tool for POSIX help texts."
    , "Usage:"
    , "  copts (parse | normalize | plot | predict) <help>"
    , "  copts -h | --help"
    ]

main :: IO ()
main = options =<< getArgs
