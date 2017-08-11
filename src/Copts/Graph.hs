module Copts.Graph (Node(..), Border, graph) where

import Text.Megaparsec
import Text.Megaparsec.String
import Algebra.Graph hiding (graph)

import Data.Foldable (foldr, foldl, concat, concatMap)
import Data.Maybe (Maybe(..))
import Data.List (delete, map, zipWith, (++), unzip3, unwords)
import Prelude (Int, Show(..), Eq, Ord(..), String, Bool(..), id, uncurry, curry, const, (&&), (.), ($))

import Copts.Applicative
import Copts.Normalizer


data Node = Text Line String | Input Line String
    deriving (Eq)

type Border = [Node]

type SubGraph = (Border, Graph Node, Border)

type Line = Int

instance Show Node where
    show (Text l t) = "[" ++ show l ++ "] " ++ t
    show (Input l t) = "[" ++ show l ++ "] " ++ t

instance Ord Node where
    (Text _ _) <= (Input _ _) = False
    (Input _ _) <= (Text _ _) = True
    (Text l t) <= (Text l' t') = (show l ++ t) <= (show l' ++ t')
    (Input l t) <= (Input l' t') = (show l ++ t) <= (show l' ++ t')


trimap f g h (a, b, c) = (f a, g b, h c)

fst (x,_,_) = x
snd (_,x,_) = x
trd (_,_,x) = x


blackhole :: Border -> Node -> Graph Node
blackhole border node = biclique border [node]

singleton :: Border -> Node -> SubGraph
singleton border node = ([node], blackhole border node, [node])

cycle :: SubGraph -> SubGraph
cycle (h, a, t) = (h, overlay a (biclique h t), h ++ t)

cartesian :: [SubGraph] -> SubGraph
cartesian subgs = trimap concat (overlays . (g :)) concat $ unzip3 subgs
    where g = overlays $ map (conn subgs) subgs
          conn as a = biclique (trd a) . concatMap fst $ delete a as


fromUsage :: Line -> Border -> Usage -> SubGraph
fromUsage l border (p:ps) = foldl conn (fromPattern l border p) ps
    where conn (h, a, t) = trimap (const h) (overlay a) id . fromPattern l t

fromParameter :: Line -> Border -> Maybe (String, Maybe String) -> SubGraph
fromParameter l b Nothing = ([], empty, [])
fromParameter l b (Just (label, Nothing)) = singleton b $ Input l label
fromParameter l b (Just (label, Just _)) = ([param], blackhole b param, param : b)
    where param = Input l label

fromPattern :: Line -> Border -> Pattern -> SubGraph
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

graph :: [Usage] -> Graph Node
graph = overlays . zipWith build [1 ..]
    where build l (p:ps) = snd $ fromUsage l (fst $ fromPattern 0 [] p) ps
