module Copts.Graph.Dot (plot) where

import Algebra.Graph
import Algebra.Graph.Export.Dot
import Data.List (length, (!!), (++))
import Prelude (String, mod, show, ($))

import Copts.Graph


print (Text l t) = show l ++ ") " ++ t
print (Input l t) = show l ++ ") " ++ t

color n = colors !! (mod n $ length colors)
    where colors = ["blue1", "cadetblue", "chocolate", "burlywood", "chartreuse",
                    "cornflowerblue", "brown", "cornsilk4", "cyan3", "red1",
                    "darkorange1", "darkorchid4", "indianred1"]

vertexStyle (Text l _) = ["color" := color l]
vertexStyle (Input l _) = ["style" := "dashed", "color" := color l]

edgeStyle _ (Text l _) = ["color" := color l]
edgeStyle _ (Input l _) = ["color" := color l]

style = Style
    { graphName               = ""
    , preamble                = ""
    , graphAttributes         = ["rankdir" := "LR"]
    , defaultVertexAttributes = ["shape" := "circle", "penwidth" := "2"]
    , defaultEdgeAttributes   = ["penwidth" := "2"]
    , vertexName              = print
    , vertexAttributes        = vertexStyle
    , edgeAttributes          = edgeStyle
    }

plot :: Graph Vertex -> String
plot = export style
