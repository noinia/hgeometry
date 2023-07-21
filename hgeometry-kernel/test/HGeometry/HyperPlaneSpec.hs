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
         it "show myHyp3" $
           show myHyp3 `shouldBe` "HyperPlane [2,1,-1]"
         it "read myHyp3" $
           read "HyperPlane [2,1,-1]" `shouldBe` myHyp3
         showReadTests
         it "same hyperplane" $ myHyp `shouldBe` myHyp2
         -- it "in halfspace" $ do
         --   mapM_ (\(q,ans) -> (q `intersects` myHalfspace) `shouldBe` ans) myPoints

         it "vertical hyperPlane through y-axis" $
           hyperPlaneThrough (Vector2 (Point2 0 (-1)) (origin :: Point 2 Int))
           `shouldBe` HyperPlane2 0 (-1) 0

         it "as nonvertical 1" $
           asNonVerticalHyperPlane (HyperPlane2 0 0 1) `shouldBe`
             Just (NonVerticalHyperPlane (Vector2 0 0))
         it "as nonvertical 2" $
           asNonVerticalHyperPlane (HyperPlane2 (-1) (-1) (-1)) `shouldBe`
             Just (NonVerticalHyperPlane (Vector2 (-1) (-1)))
         it "as nonvertical 3" $
           asNonVerticalHyperPlane (HyperPlane2 0 (-1) 0) `shouldBe`
             Nothing

         it "on side of vertical line / hyperplane" $
           (Point2 0 1 `onSideTest` HyperPlane2 3 (-1) 0)
           `shouldBe` LT
         it "on side of non-vertical line / hyperplane" $
           (Point2 0 110 `onSideTest` HyperPlane2 3 (-1) (-1))
           `shouldBe` GT
         it "on side of non-vertical line / hyperplane 2" $
           (Point2 0 (-1) `onSideTest` HyperPlane2 (-1) (-1) (-1))
           `shouldBe` EQ
         it "on side of non-vertical line / hyperplane 3" $
           (Point2 0 1 `onSideTest` HyperPlane2 (-1) (-1) (-1))
           `shouldBe` GT
         it "on side of non-vertical line / hyperplane 4" $
           (Point2 0 0 `onSideTest` HyperPlane2 (-1) (-1) (-1))
           `shouldBe` GT
         it "on side of non-vertical line / hyperplane 5" $
           (Point2 0 0 `onSideTest` HyperPlane2 (-1) (-1) (-1))
           `shouldBe` GT


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


showReadTests :: Spec
showReadTests = describe "show/read tests for" $ do
  describe "HyperPlane1" $ do
    prop "Double"       $ \(v :: HyperPlane 1 Double)         -> (read . show) v == v
    prop "Int"          $ \(v :: HyperPlane 1 Int)            -> (read . show) v == v
    prop "Rational"     $ \(v :: HyperPlane 1 Rational)       -> (read . show) v == v

  describe "HyperPlane2" $ do
    prop "Double"       $ \(v :: HyperPlane 2 Double)         -> (read . show) v == v
    prop "Int"          $ \(v :: HyperPlane 2 Int)            -> (read . show) v == v
    prop "Rational"     $ \(v :: HyperPlane 2 Rational)       -> (read . show) v == v

  describe "HyperPlane3" $ do
    prop "Double"       $ \(v :: HyperPlane 3 Double)         -> (read . show) v == v
    prop "Int"          $ \(v :: HyperPlane 3 Int)            -> (read . show) v == v
    prop "Rational"     $ \(v :: HyperPlane 3 Rational)       -> (read . show) v == v

  -- describe "HyperPlane4" $ do
  --   prop "Double"       $ \(v :: HyperPlane 4 Double)         -> (read . show) v == v
  --   prop "Int"          $ \(v :: HyperPlane 4 Int)            -> (read . show) v == v
  --   prop "Rational"     $ \(v :: HyperPlane 4 Rational)       -> (read . show) v == v




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
