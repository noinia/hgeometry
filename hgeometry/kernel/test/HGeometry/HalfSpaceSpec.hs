module HGeometry.HalfSpaceSpec
  (spec) where

import HGeometry.HalfSpace
import HGeometry.HyperPlane
import HGeometry.Intersection
import HGeometry.Kernel.Instances ()
import HGeometry.Line
import HGeometry.Point
import Test.Hspec
import Test.Hspec.QuickCheck

--------------------------------------------------------------------------------

type R = Double

myHalfspace :: HalfSpaceF (LineEQ R)
myHalfspace = HalfSpace Positive myLine

myLine :: LineEQ R
myLine = LineEQ 1 2

myPoints :: [(Point 2 R, Bool)]
myPoints = [ (Point2 10 10, False)
           , (Point2 10 1000, True)
           , (Point2 0 20, True)
           , (Point2 0 2, True)
           ]

spec :: Spec
spec = describe "halfspace Tests" $ do
         describe "in halfspace" $ do
           mapM_ (\(q,ans) -> it (show q) $ do
                     (q `intersects` myHalfspace) `shouldBe` ans) myPoints
         it "on boundary of non-vertical Positive hyperplane 2" $
           (Point2 0 (-1) `intersects` HalfSpace Positive (HyperPlane2 (-1) (-1) (-1)))
           `shouldBe` True
         it "on boundary of non-vertical Negative hyperplane 2" $
           (Point2 0 (-1) `intersects` HalfSpace Negative (HyperPlane2 (-1) (-1) (-1)))
           `shouldBe` True
         prop "normal vector points into positive halfspace" $
           \(h :: HyperPlane 2 Rational) ->
             let n = normalVector h
                 p = pointOn h
             in (p .+^ n) `intersects` HalfSpace Positive h
         prop "normal vector points into positive halfspace (NonVertical HyperPlane)" $
           \(h :: NonVerticalHyperPlane 3 Rational) ->
             let n = normalVector h
                 p = pointOn h
             in (p .+^ n) `intersects` HalfSpace Positive h
         prop "normal vector points into positive halfspace (LineEQ)" $
           \(h :: LineEQ Rational) ->
             let n = normalVector h
                 p = pointOn h
             in (p .+^ n) `intersects` HalfSpace Positive h
         -- it "intersect tests" $ do
         --   let h = HalfSpace Positive $ horizontalLine (4 % 1 :: Rational)
         --       l = LinePV origin (Vector2 (1 % 1) (1 % 1 :: Rational))
         --   ((horizontalLine @Rational $ 5 % 1) `intersects` h) `shouldBe` True
         --   (l `intersects` h) `shouldBe` True

-- test :: Ordering
-- test = (fst $ head myPoints) `onSideTest` (LineEQ 1 2)
