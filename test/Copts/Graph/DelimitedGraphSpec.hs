module Copts.Graph.DelimitedGraphSpec where

import Copts.Graph.DelimitedGraph
import Test.Hspec

import Algebra.Graph (graph)
import Data.Set (fromList)

graph1 = mandatory (singleton 1) (singleton 2)

spec = do
    it "create empty graphs" $ do
      (empty :: DelimitedGraph Int) `shouldBe` (fromList [], graph [] [], fromList [])

    it "create a graph with only one vertex" $ do
      singleton 1 `shouldBe` (fromList [1], graph [1] [], fromList [1])

    it "connect two singleton graphs" $ do
      mandatory (singleton 1) (singleton 2) `shouldBe` (fromList [1], graph [1, 2] [(1,2)], fromList [2])

    it "connect two graphs" $ do
      mandatory (singleton 0) graph1 `shouldBe` (fromList [0], graph [0, 1, 2] [(1,2), (0,1)], fromList [2])

    it "connect (optionaly) two singleton graphs" $ do
      optionaly (singleton 1) (singleton 2) `shouldBe` (fromList [1], graph [1, 2] [(1,2)], fromList [1, 2])

    it "connect (optionaly) two graphs" $ do
      optionaly (singleton 0) graph1 `shouldBe` (fromList [0], graph [0, 1, 2] [(0, 1), (1,2)], fromList [0, 2])

    it "create a loop in a singleton" $ do
      cyclical (singleton 1) `shouldBe` (fromList [1], graph [1] [(1,1)], fromList [1])

    it "create a loop" $ do
      cyclical graph1 `shouldBe` (fromList [1], graph [1,2] [(1,2), (2, 1)], fromList [1, 2])

    it "join disconnected singleton graphs" $ do
      oneOf [(singleton 1), (singleton 2)] `shouldBe` (fromList [1, 2], graph [1, 2] [], fromList [1, 2])

    it "join disconnected graphs" $ do
      oneOf [(singleton 0), graph1] `shouldBe` (fromList [0, 1], graph [0, 1, 2] [(1,2)], fromList [0, 2])

    it "cartesian connections" $ do
      cartesian [(singleton 0), graph1] `shouldBe` (fromList [0, 1], graph [0, 1, 2] [(0,1), (2,0), (1, 2)], fromList [0, 2])
