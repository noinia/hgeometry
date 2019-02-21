module Data.Geometry.HalfSpaceSpec where

import Data.Geometry.HalfSpace
import Data.Geometry.Line
import Data.Geometry
import Test.Hspec


spec :: Spec
spec = do
  describe "HalfSpace" $ do
    it "intersect tests" $ do
      let h = leftOf $ horizontalLine (4 % 1 :: Rational)
          l = Line origin (Vector2 (1 % 1) (1 % 1 :: Rational))
      ((horizontalLine $ 5 % 1) `intersects` h) `shouldBe` True
      (l `intersects` h) `shouldBe` True
