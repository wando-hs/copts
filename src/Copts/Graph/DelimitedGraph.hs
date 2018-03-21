module Copts.Graph.DelimitedGraph
    (DelimitedGraph, empty, singleton, mandatory, oneOf, optionally, cyclical, cartesian)
        where

import qualified Algebra.Graph as Alga
import qualified Data.Set as Set
import Data.Set (Set)

type DelimitedGraph a = (Set a, Alga.Graph a, Set a)

trimap f g h (a, b, c) = (f a, g b, h c)

isEmpty (inBorder, graph, outBorder) = Set.null inBorder
  && Set.null outBorder
  && Alga.isEmpty graph


empty :: DelimitedGraph a
empty = (Set.empty, Alga.empty, Set.empty)

singleton :: a -> DelimitedGraph a
singleton node = (border, Alga.vertex node, border)
    where border = Set.singleton node

mandatory :: (Ord a) => DelimitedGraph a -> DelimitedGraph a -> DelimitedGraph a
mandatory g@(inBorder, graph, outBorder) g'@(inBorder', graph', outBorder')
    | isEmpty g = g'
    | isEmpty g' = g
    | otherwise = (inBorder, Alga.simplify newGraph, outBorder')
    where connections = Alga.biclique (Set.toList outBorder) (Set.toList inBorder')
          newGraph = Alga.overlays [graph, graph', connections]

oneOf :: (Ord a) => [DelimitedGraph a] -> DelimitedGraph a
oneOf = trimap Set.unions Alga.overlays Set.unions . unzip3

optionally :: (Ord a) => DelimitedGraph a -> DelimitedGraph a -> DelimitedGraph a
optionally a b = optionalWay $ mandatory a b
    where optionalWay (inBorder, graph, outBorder) = (inBorder, graph, Set.union outBorder inBorder)

cyclical :: (Ord a) => DelimitedGraph a -> DelimitedGraph a
cyclical (inBorder, graph, outBorder) = (inBorder, newGraph, Set.union outBorder inBorder)
    where connections = Alga.biclique (Set.toList outBorder) (Set.toList inBorder)
          newGraph = Alga.overlays [graph, connections]

cartesian :: (Ord a) => [DelimitedGraph a] -> DelimitedGraph a
cartesian [] = empty
cartesian [graph] = graph
cartesian graphs = oneOf $ map (uncurry mandatory) $ removeLoops product'
    where product' = concatMap (\z -> map ((,) z) graphs) graphs
          removeLoops = filter $ uncurry (/=)

