{-# LANGUAGE ScopedTypeVariables #-}
module Data.Geometry.LineSegmentSpec where

import Data.Ext
import Data.Geometry.LineSegment
import Data.Geometry.Point
import Data.Geometry.LineSegment.Internal(onSegment)
import Test.Hspec
import Data.RealNumber.Rational

type R = RealNumber 5


spec :: Spec
spec =
  describe "onSegment" $ do
    it "handles zero length segments correctly" $ do
      let zeroSegment :: LineSegment 2 () R
          zeroSegment = ClosedLineSegment (Point2 0 0 :+ ()) (Point2 0 0 :+ ())
      (Point2 0 0 `intersects` zeroSegment) `shouldBe` True
      (Point2 1 0 `intersects` zeroSegment) `shouldBe` False

    it "2d on segment tests" $ do
      let seg1, seg2 :: LineSegment 2 () R
          seg1 = ClosedLineSegment (origin :+ ()) (Point2 2 0 :+ ())
          seg2 = ClosedLineSegment (origin :+ ()) (Point2 3 3 :+ ())
      (Point2 1    0 `intersects` seg1) `shouldBe`  True
      (Point2 1    1 `intersects` seg1) `shouldBe` False
      (Point2 5    0 `intersects` seg1) `shouldBe` False
      (Point2 (-1) 0 `intersects` seg1) `shouldBe` False
      (Point2 1    1 `intersects` seg2) `shouldBe`  True
    it "3d on segment tests" $ do
      let seg :: LineSegment 3 () R
          seg = ClosedLineSegment (origin :+ ()) (Point3 3 3 3 :+ ())
       (Point3 1 1 1 `intersects` seg) `shouldBe` True
       (Point3 1 2 1 `intersects` seg) `shouldBe` False

    it "intersecting line segment and line" $ do
      let s = ClosedLineSegment (ext $ origin) (ext $ Point2 10 (0 :: Rational))
      (s `intersect` horizontalLine (0 :: Rational)) `shouldBe` coRec s


onSegment2DSpecializationProp     :: Point 2 R -> LineSegment 2 () R -> Bool
onSegment2DSpecializationProp p s = p `onSegment` s == p `onSegment2` s
