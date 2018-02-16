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

pattern' :: Line -> DelimitedGraph Vertex -> Pattern -> DelimitedGraph Vertex
pattern' column g (Command l) = mandatory g $ singleton (Text column l)
pattern' column g (Argument l) = mandatory g $ singleton (Input column l)
pattern' column g (Option flags p) = param' column (option' column g flags) p
pattern' column g (Exclusive u) = mandatory g $ oneOf $ map (usage' column empty) u
pattern' column g (Repeated p) = mandatory g $ cyclical $ pattern' column empty p
pattern' column g (Optional u) = optionally g $ cartesian $ map (pattern' column empty) u
pattern' column g (Required u) = mandatory g $ usage' column empty u


option' column g = mandatory g . oneOf . map (singleton . Text column)

param' column g Nothing = g
param' column g (Just (l, Nothing)) = mandatory g $ singleton (Input column l)
param' column g (Just (l, _)) = optionally g $ singleton (Input column l)

usage' :: Line -> DelimitedGraph Vertex -> Usage -> DelimitedGraph Vertex
usage' column g ps = foldl (\ a b -> pattern' column a b) g ps

usage :: Line  -> Usage -> DelimitedGraph Vertex
usage line u = foldl (\ g (i,p) -> pattern' i g p) empty $ zip [1..] u

root :: Pattern -> DelimitedGraph Vertex
root node = usage 0 [node]

graph' :: Pattern -> [Usage] -> (Vertex, Alga.Graph Vertex)
graph' root' usages = (r root', g usages)
    where g = snd . oneOf . map (uncurry build) . withLines
          r = head . Set.toList . fst . root

          build line = usage line
          withLines = zip [0 ..]

          snd (a, b, c) = b
          fst (a, b, c) = a


graph :: [Usage] -> (Vertex, Alga.Graph Vertex)
graph usages = graph' (head $ head usages) usages
