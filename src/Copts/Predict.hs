module Copts.Predict (predict, predictions) where

import Algebra.Graph (Graph, edgeList, vertexList)
import Data.Map.Strict (Map, (!), fromList)
import Data.List (nub, null, find, filter, isPrefixOf)
import Data.Char (isUpper)

import Copts.Graph


reachability :: (Eq a, Ord a) => Graph a -> Map a [a]
reachability g = fromList $ map reachable vertices
    where reachable n = (n, map snd $ filter ((n ==) . fst) edges)
          vertices = vertexList g
          edges = edgeList g


match :: Vertex -> String -> Bool
match (Text _ a) param = a == param
match (Input _ _) _ = True

partialMatch :: Vertex -> String -> Bool
partialMatch (Text _ a) param = param `isPrefixOf` a && param /= a
partialMatch (Input _ _) _ = True

predict' :: Vertex -> Map Vertex [Vertex] -> [String] -> [Vertex]
predict' n m [] = [n]
predict' n m [p]
    | partialMatch n p = [n]
    | match n p = [n]
    | otherwise = []
predict' n m (p:ps)
    | match n p = let paths = concatMap (\n' -> predict' n' m ps)
                      texts = paths [t | t@Text{} <- m ! n]
                      inputs = paths [i | i@Input{} <- m ! n]
                  in if null texts then inputs else texts
    | otherwise = []

completable :: Vertex -> Bool
completable (Text _ _) = True
completable (Input _ label) = all isUpper label

predict :: [String] -> InterfaceGraph -> [Vertex]
predict [] (root, _) = [root]
predict params (root, g) = predict' root (reachability g) params

predictions :: [String] -> InterfaceGraph -> [String]
predictions params = nub . map label . filter completable . predict params
