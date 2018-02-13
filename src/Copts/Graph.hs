module Copts.Graph (Vertex(..), Line, Interface, label, line, graph) where

import qualified Algebra.Graph as Alga

import qualified Data.Set as Set

import Data.Word (Word8)

import Copts.Normalizer
import Copts.Graph.DelimitedGraph


type Line = Word8
data Vertex = Text Line String | Input Line String
    deriving (Show, Ord, Eq)


type Interface = (Vertex, Graph Vertex)

label :: Vertex -> String
label (Text _ t) = t
label (Input _ t) = t

line :: Vertex -> Line
line (Text l _) = l
line (Input l _) = l

usage :: Line -> DelimitedGraph Vertex -> Usage -> DelimitedGraph Vertex
usage line root = usage' root
    where usage' = foldl pattern'

          pattern' g (Command l) = mandatory g $ singleton (Text line l)
          pattern' g (Argument l) = mandatory g $ singleton (Input line l)
          pattern' g (Option flags p) = param' (option' g flags) p
          pattern' g (Exclusive u) = mandatory g $ oneOf $ map (usage' empty) u
          pattern' g (Repeated p) = mandatory g $ cyclical $ pattern' empty p
          pattern' g (Optional u) = optionally g $ cartesian $ map (pattern' empty) u
          pattern' g (Required u) = mandatory g $ usage' empty u

          option' g = mandatory g . oneOf . map (singleton . Text line)

          param' g Nothing = g
          param' g (Just (l, Nothing)) = mandatory g $ singleton (Input line l)
          param' g (Just (l, _)) = optionally g $ singleton (Input line l)

root :: Pattern -> DelimitedGraph Vertex
root node = usage 0 empty [node]

graph' :: Pattern -> [Usage] -> (Vertex, Alga.Graph Vertex)
graph' root' usages = (r root', g usages)
    where g = snd . oneOf . map (uncurry build) . withLines
          r = head . Set.toList . fst . root

          build line = usage line (root root')
          withLines = zip [1 ..]

          snd (a, b, c) = b
          fst (a, b, c) = a


graph :: [Usage] -> (Vertex, Alga.Graph Vertex)
graph usages = graph' (head $ head usages) (map tail usages)
