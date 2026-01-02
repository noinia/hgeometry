module Polygon.PickPointSpec where

import qualified Data.List.NonEmpty as NonEmpty
import           Data.Maybe (fromJust)
import           HGeometry.Boundary
import           HGeometry.Point
import           HGeometry.Polygon.Simple
import           Test.Hspec

--------------------------------------------------------------------------------

spec :: Spec
spec = do it "Pick point in polygon test"  $
            toSpec testPoly
          it "Pick point in polygon test"  $
            toSpec testPoly2

data TestCase r = TestCase { _polygon :: SimplePolygon (Point 2 r) }
                  deriving (Show)

toSpec                  :: TestCase Rational -> Expectation
toSpec (TestCase  poly) = ((centroid poly :: Point 2 Rational)
                           `inPolygon` poly) `shouldBe` StrictlyInside

testPoly :: TestCase Rational
testPoly = TestCase . fromJust . fromPoints . NonEmpty.fromList
         $ [origin, Point2 10 10, Point2 20 5, Point2 30 2, Point2 3 1, Point2 31 0]

testPoly2 :: TestCase Rational
testPoly2 = TestCase . fromJust . fromPoints  . NonEmpty.fromList
          $ [ Point2 208 752
            , Point2 304 688
            , Point2 224 592
            , Point2 48 736
            ]
