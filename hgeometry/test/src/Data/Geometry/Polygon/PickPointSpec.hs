{-# LANGUAGE OverloadedStrings #-}
module Data.Geometry.Polygon.PickPointSpec where

import Data.Ext
import Control.Lens
import Data.Geometry
import Data.Geometry.Polygon
import Data.Geometry.Boundary
import Test.Hspec
import           Paths_hgeometry

--------------------------------------------------------------------------------

spec :: Spec
spec = do it "Pick point in polygon test"  $
            toSpec testPoly
          it "Pick point in polygon test"  $
            toSpec testPoly2

data TestCase r = TestCase { _polygon    :: SimplePolygon () r }
                  deriving (Show)

toSpec                  :: TestCase Rational -> Expectation
toSpec (TestCase  poly) = (pickPoint poly `inPolygon` poly) `shouldBe` Inside

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
