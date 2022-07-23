module Geometry.HalfSpaceSpec where

import Geometry.Point
import Geometry.Line
import Geometry.Vector
import Geometry.HalfSpace
import Data.Intersection
import Data.Ratio
import Test.Hspec

--------------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "HalfSpace" $ do
    it "intersect tests" $ do
      let h = leftOf $ horizontalLine (4 % 1 :: Rational)
          l = Line origin (Vector2 (1 % 1) (1 % 1 :: Rational))
      ((horizontalLine @Line @Rational $ 5 % 1) `intersects` h) `shouldBe` True
      (l `intersects` h) `shouldBe` True
