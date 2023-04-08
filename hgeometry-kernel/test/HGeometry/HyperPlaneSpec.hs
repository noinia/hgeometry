module HGeometry.HyperPlaneSpec
  (spec) where

import HGeometry.HyperPlane
import HGeometry.HyperPlane.NonVertical
import HGeometry.Intersection
import HGeometry.Kernel.Instances ()
import HGeometry.Line
import HGeometry.Number.Real.Rational
import HGeometry.Point
import HGeometry.Vector
import Test.Hspec
import Test.Hspec.QuickCheck

--------------------------------------------------------------------------------

type R = RealNumber 10

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

asHyp :: ( NonVerticalHyperPlane_ hyperPlane d r
         , MkHyperPlaneConstraints d r, Num r
         ) => hyperPlane -> HyperPlane d r
asHyp = hyperPlaneFromEquation . hyperPlaneEquation

spec :: Spec
spec = describe "HyperPlane Tests" $ do
         it "same hyperplane" $ myHyp `shouldBe` myHyp2
         -- it "in halfspace" $ do
         --   mapM_ (\(q,ans) -> (q `intersects` myHalfspace) `shouldBe` ans) myPoints

         prop "intersects nonvertical conistent" $
           \(l :: LineEQ R) (m :: LineEQ R) ->
             (l `intersects` m) `shouldBe` (asHyp l `intersects` asHyp m)


         -- prop "intersect nonvertical conistent" $
         --   \(l :: LineEQ R) (m :: LineEQ R) ->
         --     (l `intersect` m) `shouldBe` (asHyp l `intersect` asHyp m)

         -- it "intersect tests" $ do
         --   let h = HalfSpace Positive $ horizontalLine (4 % 1 :: Rational)
         --       l = LinePV origin (Vector2 (1 % 1) (1 % 1 :: Rational))
         --   ((horizontalLine @Rational $ 5 % 1) `intersects` h) `shouldBe` True
         --   (l `intersects` h) `shouldBe` True

lineA, lineB :: LineEQ R
lineA = LineEQ 1.1352896048 (-1.0479630631)
lineB = LineEQ 0.8777185381 1.5392597445

hypA = asHyp lineA
hypB = asHyp lineB

test :: Bool
test = hypA `intersects` hypB

myHyp3 :: HyperPlane 2 R
myHyp3 = hyperPlaneFromEquation $ Vector3 2 1 (-1)

testz = Point2 0 2 `onHyperPlane` myHyp3
