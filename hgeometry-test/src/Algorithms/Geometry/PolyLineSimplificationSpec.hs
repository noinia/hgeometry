module Algorithms.Geometry.PolyLineSimplificationSpec (spec) where

import Algorithms.Geometry.PolyLineSimplification.ImaiIri
import Data.Ext
import Geometry.Point
import Geometry.PolyLine
import Data.Maybe
import Data.RealNumber.Rational
import Test.Hspec
import Test.QuickCheck.Instances ()

--------------------------------------------------------------------------------

type R = RealNumber 5


mkPoly :: [Point 2 R :+ Int] -> PolyLine 2 Int R
mkPoly = fromJust . fromPoints


poly1 :: PolyLine 2 Int R
poly1 = mkPoly [ origin :+ 0
               , Point2 1 1 :+ 1
               , Point2 2 (-1) :+ 2
               , Point2 3 0 :+ 3
               ]


poly2 :: PolyLine 2 Int R
poly2 = mkPoly [ origin :+ 0
               , Point2 1 1 :+ 1
               , Point2 2 (-1) :+ 2
               , Point2 3 (-2) :+ 3
               , Point2 3 0 :+ 4
               ]


answer1 :: PolyLine 2 Int R
answer1 = mkPoly [ origin :+ 0 , Point2 3 0 :+ 3]

answer2 :: PolyLine 2 Int R
answer2 = mkPoly [ origin :+ 0 , Point2 1 1 :+ 1, Point2 3 (-2) :+ 3, Point2 3 0 :+ 4]

spec :: Spec
spec = describe "ImaiIri manual" $ do
         it "simplify 1" $
           simplify 1 poly1 `shouldBe` answer1
         it "simplify 2" $
           simplify 1 poly2 `shouldBe` answer2
