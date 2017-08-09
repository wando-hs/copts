module Copts.Graph where

import Text.Megaparsec
import Text.Megaparsec.String
import Algebra.Graph

import Data.Foldable (foldr, foldl, concat, concatMap)
import Data.Maybe (Maybe(..))
import Data.List (delete, map, (++), unzip3, unwords)
import Prelude (Show, Eq, Ord(..), String, Bool(..), id, const, (.), ($))

import Copts.Applicative
import Copts.Normalizer


data Node = Text String | Input String
    deriving (Show, Eq)

type Border = [Node]

type SubGraph = (Border, Graph Node, Border)

instance Ord Node where
    (Text _) <= (Input _) = False
    (Input _) <= (Text _) = True
    (Text t) <= (Text t') = t <= t'
    (Input i) <= (Input i') = i <= i'


trimap f fromPattern h (a, b, c) = (f a, fromPattern b, h c)

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
    where g = foldr (overlay . conn subgs) empty subgs
          conn as a = biclique (trd a) . concatMap fst $ delete a as


fromUsage :: Border -> Usage -> SubGraph
fromUsage border (p:ps) = foldl conn (fromPattern border p) ps
    where conn (h, a, t) = trimap (const h) (overlay a) id . fromPattern t

fromParameter :: Border -> Maybe (String, Maybe String) -> SubGraph
fromParameter b Nothing = (b, empty, b)
fromParameter b (Just (label, Nothing)) = singleton b $ Input label
fromParameter b (Just (label, Just _)) = ([param], blackhole b param, param : b)
    where param = Input label

fromPattern :: Border -> Pattern -> SubGraph
fromPattern b (Argument label) = singleton b $ Input label

fromPattern b (Command name) = singleton b $ Text name

fromPattern border (Option fs p) = ([n], overlays gs, e)
    where (w, param, e) = fromParameter [n] p
          n = Text $ unwords fs
          gs = [param, (blackhole border n), (star n w)]

fromPattern border (Required u) = fromUsage border u

fromPattern border (Repeated p) =  cycle $ fromPattern border p

fromPattern border (Exclusive us) = trimap concat overlays concat $ unzip3 $ map (fromUsage border) us

fromPattern border (Optional u) = cartesian $ map (fromPattern border) u


graph :: Usage -> Graph Node
graph = snd . fromUsage []
