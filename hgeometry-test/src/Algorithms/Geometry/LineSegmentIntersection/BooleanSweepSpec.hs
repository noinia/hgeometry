{-# LANGUAGE OverloadedStrings #-}
module Algorithms.Geometry.LineSegmentIntersection.BooleanSweepSpec (spec) where

import qualified Algorithms.Geometry.LineSegmentIntersection.BooleanSweep   as Sweep
import qualified Algorithms.Geometry.LineSegmentIntersection.Naive          as Naive
import           Data.Ext
import           Data.Geometry.LineSegment
import           Data.Geometry.Point
import           Data.RealNumber.Rational
import           Test.Hspec
import           Test.QuickCheck

type R = RealNumber 5

open,closed :: R -> R -> R -> R -> LineSegment 2 () R
open x1 y1 x2 y2 = OpenLineSegment (ext $ Point2 x1 y1) (ext $ Point2 x2 y2)
closed x1 y1 x2 y2 = ClosedLineSegment (ext $ Point2 x1 y1) (ext $ Point2 x2 y2)

line1  = closed 0 0   1 0
line2  = closed 0 0   0 1

line3  = open   0 0   1 0
line4  = open   0 0   0 1

line5  = closed 0 0   1 1
line6  = open   0.5 0.5   0.5 0
line7  = closed 0.5 0.5   0.5 0

line8  = closed 10 10     0 0
line9  = closed 1  10     0 0
line10 = closed 10  1     0 0

line8'  = open  10 10     0 0
line9'  = open  1 10      0 0
line10' = open  10 1      0 0

line11 = closed 0 0    2 0
line12 = closed 1 0    3 0

line13 = open   0 0    2 0
line14 = open   2 0    4 0

spec :: Spec
spec = do
  it "doesn't overlap" $
    Sweep.intersections [] `shouldBe` False
  it "end-point overlap" $
    Sweep.intersections [line1, line2] `shouldBe` True
  it "open endpoints" $
    Sweep.intersections [line3, line4] `shouldBe` False
  it "open endpoints / midpoint" $
    Sweep.intersections [line5, line6] `shouldBe` False
  it "closed endpoints / midpoint" $
    Sweep.intersections [line5, line7] `shouldBe` True
  it "same endpoint" $
    Sweep.intersections [line8, line9, line10] `shouldBe` True
  it "same endpoint / open" $
    Sweep.intersections [line8', line9', line10'] `shouldBe` False
  it "horizontal overlap" $
    Sweep.intersections [line11, line12] `shouldBe` True
  it "horizontal near overlap" $
    Sweep.intersections [line13, line14] `shouldBe` False
  it "matches naive" $
    property $ \(lst :: [LineSegment 2 () R]) ->
      Sweep.intersections lst === not (null $ Naive.intersections lst)
