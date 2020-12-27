{-# LANGUAGE ScopedTypeVariables #-}
module Data.Geometry.LineSegmentSpec where

import Data.Ext
import Data.Geometry.Line
import Data.Geometry.LineSegment
import Data.Geometry.LineSegment.Internal (onSegment)
import Data.Geometry.Point
import Data.Geometry.Vector
import Data.Intersection
import Data.RealNumber.Rational
import Data.Vinyl.CoRec
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Instances ()



type R = RealNumber 5


spec :: Spec
spec =
  describe "onSegment" $ do
    let intersects' :: Arity d => Point d R -> LineSegment d () R -> Bool
        intersects' = intersects

    it "handles zero length segments correctly" $ do
      let zeroSegment = ClosedLineSegment (Point2 0 0 :+ ()) (Point2 0 0 :+ ())
      (Point2 0 0 `intersects'` zeroSegment) `shouldBe` True
      (Point2 1 0 `intersects'` zeroSegment) `shouldBe` False

    it "2d on segment tests" $ do
      let seg1 = ClosedLineSegment (origin :+ ()) (Point2 2 0 :+ ())
          seg2 = ClosedLineSegment (origin :+ ()) (Point2 3 3 :+ ())
      (Point2 1    0 `intersects'` seg1) `shouldBe`  True
      (Point2 1    1 `intersects'` seg1) `shouldBe` False
      (Point2 5    0 `intersects'` seg1) `shouldBe` False
      (Point2 (-1) 0 `intersects'` seg1) `shouldBe` False
      (Point2 1    1 `intersects'` seg2) `shouldBe`  True

    it "3d on segment tests" $ do
      let seg = ClosedLineSegment (origin :+ ()) (Point3 3 3 3 :+ ())
      (Point3 1 1 1 `intersects'` seg) `shouldBe` True
      (Point3 1 2 1 `intersects'` seg) `shouldBe` False

    it "onSegment2 same result as generic onSegment (quickheck)" $
      property $ \(q :: Point 2 R) (seg :: LineSegment 2 () R) ->
        q `onSegment` seg == q `onSegment2` seg
      -- FIXME: we should probably generate specific points on the segment instead.

    it "intersecting line segment and line" $ do
      let s = ClosedLineSegment (ext origin) (ext $ Point2 10 (0 :: R))
      (s `intersect` horizontalLine (0 :: R)) `shouldBe` coRec s
