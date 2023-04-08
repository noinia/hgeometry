module HGeometry.HyperPlaneSpec
  (spec) where

import HGeometry.HyperPlane
import HGeometry.HyperPlane.NonVertical
import HGeometry.Intersection
import HGeometry.Line
import HGeometry.Point
import HGeometry.Vector
import Test.Hspec

--------------------------------------------------------------------------------

type R = Double

myHyp :: NonVerticalHyperPlane 2 R
myHyp = NonVerticalHyperPlane $ Vector2 1 2
-- should be the same as myLine

myHyp2 :: NonVerticalHyperPlane 2 R
myHyp2 = hyperPlaneFromEquation $ Vector3 2 1 (-1)
-- should be the same as myLine



myLine :: LineEQ R
myLine = LineEQ 1 2

myPoints :: [(Point 2 R, Bool)]
myPoints = [ (Point2 10 10, False)
           , (Point2 10 1000, True)
           , (Point2 0 20, True)
           , (Point2 0 2, True)
           ]

spec :: Spec
spec = describe "HyperPlane Tests" $ do
         it "same hyperplane" $ myHyp `shouldBe` myHyp2
         -- it "in halfspace" $ do
         --   mapM_ (\(q,ans) -> (q `intersects` myHalfspace) `shouldBe` ans) myPoints


         -- it "intersect tests" $ do
         --   let h = HalfSpace Positive $ horizontalLine (4 % 1 :: Rational)
         --       l = LinePV origin (Vector2 (1 % 1) (1 % 1 :: Rational))
         --   ((horizontalLine @Rational $ 5 % 1) `intersects` h) `shouldBe` True
         --   (l `intersects` h) `shouldBe` True

test :: Ordering
test = (fst $ head myPoints) `onSideTest` (LineEQ 1 2)
