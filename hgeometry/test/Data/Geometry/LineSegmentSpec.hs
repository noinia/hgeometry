{-# LANGUAGE ScopedTypeVariables #-}
module Data.Geometry.LineSegmentSpec where

import Data.Ext
import Data.Geometry
import Data.Vinyl.CoRec
import Test.Hspec

spec :: Spec
spec =
  describe "onSegment" $ do
    it "handles zero length segments correctly" $ do
      let zeroSegment :: LineSegment 2 () Rational
          zeroSegment = ClosedLineSegment (Point2 0 0 :+ ()) (Point2 0 0 :+ ())
      (Point2 0 0 `onSegment` zeroSegment) `shouldBe` True
      (Point2 1 0 `onSegment` zeroSegment) `shouldBe` False
    it "intersecting line segment and line" $ do
      let s = ClosedLineSegment (ext $ origin) (ext $ Point2 10 (0 :: Rational))
      (s `intersect` horizontalLine (0 :: Rational)) `shouldBe` coRec s
