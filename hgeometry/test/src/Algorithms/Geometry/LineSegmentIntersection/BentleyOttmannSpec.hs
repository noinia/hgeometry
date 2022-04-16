{-# LANGUAGE OverloadedStrings          #-}
module Algorithms.Geometry.LineSegmentIntersection.BentleyOttmannSpec (spec) where

import           Algorithms.Geometry.LineSegmentIntersection
import qualified Algorithms.Geometry.LineSegmentIntersection.BentleyOttmann as Sweep
import qualified Algorithms.Geometry.LineSegmentIntersection.Naive as Naive
import           Control.Lens
import           Data.Ext
import           Geometry.LineSegment
import           Geometry.Polygon
import           Data.List (intercalate)
import qualified Data.Map as Map
import           Data.Proxy
import           Data.RealNumber.Rational
import           Paths_hgeometry
import           System.FilePath.Lens
import           Test.Hspec

type R = RealNumber 5


spec :: Spec
spec = pure ()

  -- do
  -- describe "Testing Bentley Ottmann LineSegment Intersection" $ do
  --   -- toSpec (TestCase "myPoints" myPoints)
  --   -- toSpec (TestCase "myPoints'" myPoints')


-- | Test if we have the same intersection points
samePointsAsNaive segs = it "Same points as Naive" $ do
  (Map.keys $ Sweep.intersections segs) `shouldBe` (Map.keys $ Naive.intersections segs)

-- | Test if they every intersection point has the right segments
sameAsNaive      :: (Fractional r, Ord r, Eq p, Show e
                    , Show p, Show r
                    ) => [LineSegment 2 p r :+ e] -> Spec
sameAsNaive segs = it "Same as Naive " $ do
    (Sweep.intersections segs) `shouldBe` (Naive.intersections segs)
