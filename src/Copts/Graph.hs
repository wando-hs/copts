module Copts.Graph (Vertex(..), Column, Interface, label, graph) where

import qualified Algebra.Graph as Alga

import qualified Data.Set as Set

import Data.Word (Word8)

import Copts.AST
import Copts.Graph.DelimitedGraph


type Column = Word8
data Vertex = Text Column String | Input Column String
    deriving (Show, Ord, Eq)


type Interface = (Vertex, Alga.Graph Vertex)


text (Short  t) = [ '-', t ]
text (Long  t) = "--" ++  t

patternToGraph root column = pattern' root
    where usage' = foldr (flip pattern')

          option' g = mandatory g . oneOf . map (singleton . Text column)

          param' g Nothing = g
          param' g (Just (Parameter l Nothing)) = mandatory g $ singleton (Input column l)
          param' g (Just (Parameter l _)) = optionally g $ singleton (Input column l)

          pattern' g (Command l) = mandatory g $ singleton (Text column l)
          pattern' g (Argument l) = mandatory g $ singleton (Input column l)
          pattern' g (Option flags p) = param' (option' g (map text flags)) p
          pattern' g (Exclusive u) = mandatory g $ oneOf $ map (usage' empty) u
          pattern' g (Repeated p) = mandatory g $ cyclical $ pattern' empty p
          pattern' g (Optional u) = optionally g $ cartesian $ map (pattern' empty) u
          pattern' g (Required u) = mandatory g $ usage' empty u

usageToGraph = foldr convert empty . withColumns
    where convert (column, pattern') g = patternToGraph g column pattern'
          withColumns = reverse . zip [0..]


graph :: [Usage] -> (Vertex, Alga.Graph Vertex)
graph usages = (root usages, graph' usages)
    where graph' = snd' . oneOf . map usageToGraph
          root = head . Set.toList . fst' . usageToGraph . pure . head . head

          snd' (_, b, _) = b
          fst' (a, _, _) = a

label :: Vertex -> String
label (Text _ t) = t
label (Input _ t) = t
