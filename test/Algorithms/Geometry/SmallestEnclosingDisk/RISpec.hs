module Algorithms.Geometry.SmallestEnclosingDisk.RISpec where

import Test.Hspec

import Data.Geometry
import Algorithms.Geometry.SmallestEnclosingBall.Types
import qualified Algorithms.Geometry.SmallestEnclosingBall.RandomizedIncrementalConstruction as RIC
import qualified Algorithms.Geometry.SmallestEnclosingBall.Naive as Naive

spec :: Spec
spec = do
  describe "Testing Smallest Enclosing disk using RIC" $ do
    it "returns the first element of a list" $ do
      head [23 ..] `shouldBe` (23 :: Int)
