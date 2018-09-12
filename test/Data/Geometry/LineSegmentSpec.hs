{-# LANGUAGE ScopedTypeVariables #-}
module Data.Geometry.LineSegmentSpec where

import           Data.Ext
import           Data.Geometry
import           Test.Hspec
import           Test.QuickCheck.HGeometryInstances ()

spec :: Spec
spec =
  describe "onSegment" $
    it "handles zero length segments correctly" $ do
        let zeroSegment :: LineSegment 2 () Rational
            zeroSegment = ClosedLineSegment (Point2 0 0 :+ ()) (Point2 0 0 :+ ())
        (Point2 0 0 `onSegment` zeroSegment) `shouldBe` True
        (Point2 1 0 `onSegment` zeroSegment) `shouldBe` False
