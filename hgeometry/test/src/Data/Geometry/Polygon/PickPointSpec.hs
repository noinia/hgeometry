{-# LANGUAGE OverloadedStrings #-}
module Data.Geometry.Polygon.PickPointSpec where

import Control.Lens
import Data.Ext
import Data.Geometry
import Data.Geometry.Boundary
import Data.Geometry.Polygon
import Paths_hgeometry
import Test.Hspec

--------------------------------------------------------------------------------

spec :: Spec
spec = do it "Pick point in polygon test"  $
            toSpec testPoly
          it "Pick point in polygon test"  $
            toSpec testPoly2

testPoly :: TestCase Rational
testPoly = TestCase . toCounterClockWiseOrder. fromPoints . map ext
         $ [origin, Point2 10 10, Point2 20 5, Point2 30 2, Point2 3 1, Point2 31 0]

testPoly2 :: TestCase Rational
testPoly2 = TestCase . toCounterClockWiseOrder. fromPoints . map ext
          $ [ Point2 208 752
            , Point2 304 688
            , Point2 224 592
            , Point2 48 736
            ]

-- main = readInputFromFile "tests/Data/Geometry/pointInPolygon.ipe"
