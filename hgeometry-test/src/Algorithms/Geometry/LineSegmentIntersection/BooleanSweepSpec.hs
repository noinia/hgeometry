{-# LANGUAGE OverloadedStrings          #-}
module Algorithms.Geometry.LineSegmentIntersection.BooleanSweepSpec (spec) where

import           Algorithms.Geometry.LineSegmentIntersection
import qualified Algorithms.Geometry.LineSegmentIntersection.BooleanSweep as Sweep
import qualified Algorithms.Geometry.LineSegmentIntersection.Naive as Naive
import qualified Algorithms.Geometry.LineSegmentIntersection.BentleyOttmann as BO
import           Control.Lens
import           Data.Ext
import           Data.Geometry.Ipe
import           Data.Geometry.LineSegment
import           Data.Geometry.Point
import           Data.Geometry.Polygon
import qualified Data.Map as Map
import           Data.Proxy
import           Paths_hgeometry_test
import           Test.Hspec
import           Test.QuickCheck
import Data.RealNumber.Rational

type R = RealNumber 5

line1, line2, line3, line4 :: LineSegment 2 () R

line1 = ClosedLineSegment (ext $ Point2 0 0) (ext $ Point2 1 0)
line2 = ClosedLineSegment (ext $ Point2 0 0) (ext $ Point2 0 1)

line3 = OpenLineSegment (ext $ Point2 0 0) (ext $ Point2 1 0)
line4 = OpenLineSegment (ext $ Point2 0 0) (ext $ Point2 0 1)

spec :: Spec
spec = do
  it "doesn't overlap" $
    Sweep.intersections [] `shouldBe` False
  it "end-point overlap" $
    Sweep.intersections [line1, line2] `shouldBe` True
  it "open endpoints" $
    Sweep.intersections [line3, line4] `shouldBe` False
  -- it "matches naive" $
  --   property $ \(lst :: [LineSegment 2 () R]) ->
  --     not (null $ BO.intersections lst) === not (null $ Naive.intersections lst)
  it "matches naive" $
    property $ \(lst :: [LineSegment 2 () R]) ->
      Sweep.intersections lst === not (null $ Naive.intersections lst)
