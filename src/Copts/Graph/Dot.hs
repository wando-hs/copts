module Copts.Graph.Dot (plot) where

import Algebra.Graph (Graph)
import Algebra.Graph.Export.Dot
import Data.List (length, (!!), (++))
import Prelude (String, mod, show, ($), fromIntegral)

import Copts.Graph


color n = colors !! index
  where index = mod (fromIntegral n) (length colors)
        colors = ["blue1", "cadetblue", "chocolate", "burlywood", "chartreuse"
                 , "cornflowerblue", "brown", "cornsilk4", "cyan3", "red1"
                 , "darkorange1", "darkorchid4", "indianred1"]

name (Text column text) = show column ++ "-" ++ text
name (Input column text) = show column ++ "-" ++ text

vertex (Text column text) = ["color" := color column, "label" := text]
vertex (Input column text) = [ "color" := color column
                             , "label" := text
                             , "style" := "dashed"]


plot :: Graph Vertex -> String
plot = export $ Style
  { graphName               = ""
  , preamble                = ""
  , graphAttributes         = ["rankdir" := "LR"]
  , defaultVertexAttributes = ["shape" := "circle", "penwidth" := "2"]
  , defaultEdgeAttributes   = ["penwidth" := "3", "color" := "gray"]
  , vertexName              = name
  , vertexAttributes        = vertex
  , edgeAttributes          = \ _ _ -> []
  }
