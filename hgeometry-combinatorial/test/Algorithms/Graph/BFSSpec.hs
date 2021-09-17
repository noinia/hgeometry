module Algorithms.Graph.BFSSpec (spec) where

import           Algorithms.Graph.BFS
import           Data.Tree
import qualified Data.Vector as V
import           Test.Hspec

--------------------------------------------------------------------------------

spec :: Spec
spec = it "BFS test" $
         bfs' 0 testGr `shouldBe` answer

testGr :: V.Vector [Int]
testGr = V.fromList [ [1,3, 6]
                    , [2]
                    , [3, 5]
                    , [2 ]
                    , []
                    , [4, 5]
                    , [7]
                    , [8,4]
                    , [2]
                    ]
answer :: Tree Int
answer = Node {rootLabel = 0, subForest = [Node {rootLabel = 1, subForest = [Node {rootLabel = 2, subForest = [Node {rootLabel = 5, subForest = []}]}]},Node {rootLabel = 3, subForest = []},Node {rootLabel = 6, subForest = [Node {rootLabel = 7, subForest = [Node {rootLabel = 8, subForest = []},Node {rootLabel = 4, subForest = []}]}]}]}
