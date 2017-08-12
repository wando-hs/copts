module Copts.Graph (Vertex(..), InterfaceGraph, graph) where

import Text.Megaparsec
import Text.Megaparsec.String
import Algebra.Graph (connect, connects, overlay, overlays, empty, star, biclique)
import qualified Algebra.Graph as Alga

import Prelude hiding (cycle, fst, snd)

import Data.Foldable (foldr, foldl, concat, concatMap)
import Data.Maybe (Maybe(..))
import Data.List (delete, map, zipWith, (++), unzip3, unwords)

import Copts.Applicative
import Copts.Normalizer


type InnerGraph = (Border, Graph, Border)
type Border = [Vertex]

type Graph = Alga.Graph Vertex
type Line = Int


type Root = Vertex
type InterfaceGraph = (Root, Graph)

data Vertex = Text Line String | Input Line String
    deriving (Show, Eq)

instance Ord Vertex where
    node1 <= node2 = content node1 <= content node2


content (Text l t) = show l ++ t
content (Input l t) = show l ++ t

trimap f g h (a, b, c) = (f a, g b, h c)

fst (x,_,_) = x
snd (_,x,_) = x
trd (_,_,x) = x


blackhole :: Border -> Vertex -> Graph
blackhole border node = biclique border [node]

singleton :: Border -> Vertex -> InnerGraph
singleton border node = ([node], blackhole border node, [node])

cycle :: InnerGraph -> InnerGraph
cycle (h, a, t) = (h, overlay a (biclique h t), h ++ t)

cartesian :: [InnerGraph] -> InnerGraph
cartesian subgs = trimap concat (overlays . (g :)) concat $ unzip3 subgs
    where g = overlays $ map (conn subgs) subgs
          conn as a = biclique (trd a) . concatMap fst $ delete a as


fromUsage :: Line -> Border -> Usage -> InnerGraph
fromUsage l border (p:ps) = foldl conn (fromPattern l border p) ps
    where conn (h, a, t) = trimap (const h) (overlay a) id . fromPattern l t

fromParameter :: Line -> Border -> Maybe (String, Maybe String) -> InnerGraph
fromParameter l b Nothing = ([], empty, [])
fromParameter l b (Just (label, Nothing)) = singleton b $ Input l label
fromParameter l b (Just (label, Just _)) = ([param], blackhole b param, param : b)
    where param = Input l label

fromPattern :: Line -> Border -> Pattern -> InnerGraph
fromPattern l b (Argument label) = singleton b $ Input l label

fromPattern l b (Command name) = singleton b $ Text l name

fromPattern l border (Option fs p) = ([n], overlays gs, e)
    where (w, param, e) = fromParameter l [n] p
          n = Text l $ unwords fs
          gs = [param, blackhole border n, star n w]

fromPattern l border (Required u) = fromUsage l border u

fromPattern l border (Repeated p) =  cycle $ fromPattern l border p

fromPattern l border (Exclusive us) = trimap concat overlays concat $ unzip3 $ map (fromUsage l border) us

fromPattern l border (Optional u) = trimap id id (border ++) $ cartesian $ map (fromPattern l border) u

rootVertex :: [Usage] -> Vertex
rootVertex = head . fst . fromPattern 0 [] . head . head

graph :: [Usage] -> InterfaceGraph
graph us = (root, overlays $ zipWith build [1 ..] us)
    where build l = snd . fromUsage l [root] . tail
          root = rootVertex us
