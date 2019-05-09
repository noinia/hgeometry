module Data.Geometry.HalfSpaceSpec where

import Data.Geometry
import Data.Geometry.HalfSpace
import Data.Geometry.Line
import Data.Ratio
import Test.Hspec

--------------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "HalfSpace" $ do
    it "intersect tests" $ do
      let h = leftOf $ horizontalLine (4 % 1 :: Rational)
          l = Line origin (Vector2 (1 % 1) (1 % 1 :: Rational))
      ((horizontalLine @Rational $ 5 % 1) `intersects` h) `shouldBe` True
      (l `intersects` h) `shouldBe` True
