module Copts.Predict (predict) where

import Algebra.Graph (Graph, edgeList, vertexList)
import Data.Map.Strict (Map, (!), fromList)
import Data.List (find, filter)

import Copts.Graph


reachability :: (Eq a, Ord a) => Graph a -> Map a [a]
reachability g = fromList $ map reachable vertices
    where reachable n = (n, map snd $ filter ((n ==) . fst) edges)
          vertices = vertexList g
          edges = edgeList g


match :: Node -> String -> Bool
match (Text _ a) param = a == param
match (Input _ _) _ = True

predict' :: Node -> Map Node [Node] -> [String] -> [Node]
predict' n m [p] = if match n p then m ! n else []
predict' n m (p:ps)
  | match n p = concatMap (\n' -> predict' n' m ps) $ m ! n
  | otherwise = []


predict :: [String] -> Graph Node -> [Node]
predict (n:ns) g = predict' root (reachability g) $ n:ns
    where root = Text 0 n
