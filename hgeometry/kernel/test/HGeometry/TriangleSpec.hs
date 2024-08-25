{-# LANGUAGE OverloadedStrings #-}
module HGeometry.TriangleSpec
  ( spec
  ) where

import Control.Lens
import HGeometry.Boundary
import HGeometry.Intersection
import HGeometry.Interval
-- import HGeometry.Line
-- import HGeometry.LineSegment
import HGeometry.Number.Real.Rational
import HGeometry.Point
import HGeometry.Triangle
import HGeometry.Vector
import HGeometry.Kernel.Instances()
import Hiraffe.Graph
import Test.Hspec
import Test.Hspec.QuickCheck
-- import Test.QuickCheck
import Test.QuickCheck.Instances ()

--------------------------------------------------------------------------------

type R = RealNumber 10

spec :: Spec
spec = describe "intersection tests" $ do
            -- it "intersecting Line 2 with Triangle 2 " $ do
            --   let t :: Triangle (Point 2 Rational)
            --       t = Triangle origin (Point2 10 0) (Point2 10 10)
            --       hor :: Rational -> Line 2 Rational
            --       hor = horizontalLine
            --   (hor 3 `intersect` t)
            --     `shouldBe` (coRec $ ClosedLineSegment (ext $ Point2 10 (3 :: Rational))
            --                                           (ext $ Point2 3  (3 :: Rational)))
            --   (hor 10 `intersect` t)
            --     `shouldBe` (coRec $ Point2 10 (10 :: Rational))
            --   (hor 11 `intersect` t)
            --     `shouldBe` Nothing
          -- prop "inTriangle same as inTriangleFrac" $
          --   \(q :: Point 2 R) (t :: Triangle (Point 2 R)) ->
          --     (q `inTriangle` t) `shouldBe` (q `inTriangleFrac` t)
          prop "onTriangle same as onTriangleFrac" $ \(q :: Point 2 R)
                                                      (t :: Triangle (Point 2 R)) ->
              (q `intersects` t) `shouldBe` (q `onTriangleFrac` t)
          -- TODO: this test probably does not produce many onBoundaries
          prop "vertices on triangle" $ \(t :: Triangle (Point 2 R)) ->
              allOf vertices (\v -> v `intersects` t) t

          it "manual intersect " $
            let t = Triangle origin (Point2 0 (-1)) (Point2 (-1) 0) :: Triangle (Point 2 Int)
            in ( Point2 0 0    `intersects` t
               , Point2 0 (-1) `intersects` t
               , Point2 (-1) 0 `intersects` t
               ) `shouldBe` (True,True,True)
          it "manual intersect halfspaces" $
            let t = Triangle origin (Point2 0 (-1)) (Point2 (-1) 0) :: Triangle (Point 2 Int)
                Vector3 a b c = intersectingHalfPlanes t
            in ( Point2 (-1) 0 `intersects` a
               , Point2 (-1) 0 `intersects` b
               , Point2 (-1) 0 `intersects` c
               ) `shouldBe` (True,True,True)
          it "intersecting halfspaces" $
            let t = Triangle origin (Point2 0 (-1)) (Point2 (-1) 0) :: Triangle (Point 2 Int)
            in (show <$> intersectingHalfPlanes t) `shouldBe`
               (Vector3 "HalfSpace Positive (LinePV (Point2 0 0) (Vector2 (-1) 0))"
                        "HalfSpace Positive (LinePV (Point2 (-1) 0) (Vector2 1 (-1)))"
                        "HalfSpace Positive (LinePV (Point2 0 (-1)) (Vector2 0 1))"
               )

--------------------------------------------------------------------------------

_inTriangleFrac   :: (Ord r, Fractional r)
                 => Point 2 r -> Triangle (Point 2 r) -> PointLocationResult
_inTriangleFrac q t
    | all (`inRange` OpenInterval   0 1) [a,b,c] = Inside
    | all (`inRange` ClosedInterval 0 1) [a,b,c] = OnBoundary
    | otherwise                                  = Outside
  where
    Vector3 a b c = toBarricentric q t

-- | Test if a point lies inside or on the boundary of a triangle
onTriangleFrac       :: (Ord r, Fractional r)
                     => Point 2 r -> Triangle (Point 2 r) -> Bool
q `onTriangleFrac` t = let Vector3 a b c = toBarricentric q t
                       in all (`inRange` ClosedInterval 0 1) [a,b,c]


inRange :: (Ord r, Interval_ interval r) => r -> interval -> Bool
q `inRange` i = q `inInterval` i /= Outside
