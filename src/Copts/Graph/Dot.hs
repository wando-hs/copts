module Copts.Graph.Dot (plot) where

import Copts.Graph
import Algebra.Graph
import Algebra.Graph.Export.Dot

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
    , vertexName              = show
    , vertexAttributes        = vertexStyle
    , edgeAttributes          = edgeStyle
    }

plot :: Graph Node -> String
plot = export style
