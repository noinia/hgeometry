{-# LANGUAGE ScopedTypeVariables #-}
module Data.Geometry.LineSegmentSpec where

import           Data.Ext
import qualified Data.Foldable                      as F
import           Data.Geometry
import           Data.Geometry.Box
import           Data.Geometry.LineSegment
import           Data.Geometry.Point
import qualified Data.Seq                           as Seq
import qualified Data.Set                           as Set
import           GHC.TypeLits
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.HGeometryInstances ()

spec :: Spec
spec =
  describe "onSegment" $
    it "handles zero length segments correctly" $ do
        let zeroSegment :: LineSegment 2 () Double
            zeroSegment = ClosedLineSegment (Point2 0.0 0.0 :+ ()) (Point2 0.0 0.0 :+ ())
        (Point2 0 0 `onSegment` zeroSegment) `shouldBe` True
        (Point2 1 0 `onSegment` zeroSegment) `shouldBe` False
