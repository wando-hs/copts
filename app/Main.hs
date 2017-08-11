module Main where


import Text.Megaparsec (parseMaybe, parse)
import System.Environment (getArgs)
import Algebra.Graph (edgeList, overlays)
import Algebra.Graph.Export.Dot
import Data.Maybe

import Copts.Graph (Node(..), graph, graph')
import Copts.Normalizer (normalize)
import Copts.Parser (help)

print' :: Show a => a -> IO ()
print' = putStrLn . show

options :: [String] -> IO ()
options ("parse":text:_) = print . show $ parse help "" text
options ("normalize":text:_) = print' $ normalize <$> parseMaybe help text
--options ("graph":i:text:_) = fromJust $ putStrLn <$> export style <$> graph <$> (\a -> a !! (read i)) <$> normalize <$> parseMaybe help text
options ("graph":_:text:_) = fromJust $ putStrLn <$> export style <$> graph' <$> normalize <$> parseMaybe help text

options x = mempty

main = options =<< getArgs


vertexStyle (Text _) = ["color" := "green"]
vertexStyle (Input _) = ["color" := "blue"]

style :: Style Node String
style = Style
    { graphName               = ""
      , preamble                = ""
      , graphAttributes         = ["rankdir" := "LR"]
      , defaultVertexAttributes = ["shape" := "circle"]
      , defaultEdgeAttributes   = mempty
      , vertexName              = show
      , vertexAttributes        = vertexStyle
      , edgeAttributes          = (const . const) ["color" := "gray"] }
