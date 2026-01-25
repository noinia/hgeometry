module PolyLine.Simplification.ImaiIriSpec
  (spec) where

import qualified Data.List.NonEmpty as NonEmpty
import           HGeometry.Ext
import           R
import           HGeometry.Point
import           HGeometry.PolyLine
import           HGeometry.PolyLine.Simplification.ImaiIri
import           Test.Hspec
import           Test.QuickCheck.Instances ()

--------------------------------------------------------------------------------

mkPoly :: [Point 2 R :+ Int] -> PolyLine (Point 2 R :+ Int)
mkPoly = polyLineFromPoints . NonEmpty.fromList


poly1 :: PolyLine (Point 2 R :+ Int)
poly1 = mkPoly [ origin :+ 0
               , Point2 1 1 :+ 1
               , Point2 2 (-1) :+ 2
               , Point2 3 0 :+ 3
               ]


poly2 :: PolyLine (Point 2 R :+ Int)
poly2 = mkPoly [ origin :+ 0
               , Point2 1 1 :+ 1
               , Point2 2 (-1) :+ 2
               , Point2 3 (-2) :+ 3
               , Point2 3 0 :+ 4
               ]


answer1 :: PolyLine (Point 2 R :+ Int)
answer1 = mkPoly [ origin :+ 0 , Point2 3 0 :+ 3]

answer2 :: PolyLine (Point 2 R :+ Int)
answer2 = mkPoly [ origin :+ 0 , Point2 1 1 :+ 1, Point2 3 (-2) :+ 3, Point2 3 0 :+ 4]

spec :: Spec
spec = describe "ImaiIri manual" $ do
         it "simplify 1" $
           simplify 1 poly1 `shouldBe` answer1
         it "simplify 2" $
           simplify 1 poly2 `shouldBe` answer2
