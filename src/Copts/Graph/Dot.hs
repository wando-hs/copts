module Copts.Graph.Dot (plot) where

import Algebra.Graph
import Algebra.Graph.Export.Dot
import Data.List (length, (!!), (++))
import Prelude (String, mod, show, ($), fromIntegral)

import Copts.Graph



print (Text l t) = show l ++ "-" ++ t
print (Input l t) = show l ++ "-" ++ t

color :: Line -> String
color n = colors !! fromIntegral (mod n $ fromIntegral $ length colors)
    where colors = ["blue1", "cadetblue", "chocolate", "burlywood", "chartreuse",
                    "cornflowerblue", "brown", "cornsilk4", "cyan3", "red1",
                    "darkorange1", "darkorchid4", "indianred1"]

vertexStyle (Text l t) = ["color" := color l, "label" := t]
vertexStyle (Input l t) = ["style" := "dashed", "color" := color l, "label" := t]

edgeStyle _ _ = ["color" := "gray"]

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
