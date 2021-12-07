module Algorithms.Graph.DFSSpec (spec) where


import           Algorithms.Graph.DFS  (dfsSensitive)
import           Control.DeepSeq       (NFData (rnf))
import           Control.Exception     (evaluate)
import qualified Data.Foldable         as F
import           Data.PlanarGraph (VertexId (VertexId), World (Primal))
import qualified Data.Set              as S
import           Data.Tree             (Tree (rootLabel, subForest))
import           Test.Hspec            (Spec, anyErrorCall, describe, it, shouldBe, shouldThrow)

{-
  0
 / \
1   2
-}
tree1 :: Tree (VertexId () Primal)
tree1 = dfsSensitive (\i -> case i of VertexId 0 -> [VertexId 1, VertexId 2]; _ -> error "fail!") (VertexId 0)

spec :: Spec
spec = do
  describe "DFS output sensitivity" $ do
    it "fails when forced" $
      evaluate (rnf tree1) `shouldThrow` anyErrorCall
    it "outputs root label lazily" $
      rootLabel tree1 `shouldBe` VertexId 0
    it "outputs forest lazily" $
      length (subForest tree1) `shouldBe` 2
