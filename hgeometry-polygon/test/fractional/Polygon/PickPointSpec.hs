module Polygon.PickPointSpec where

import qualified Data.List.NonEmpty as NonEmpty
import           Data.Maybe (fromJust)
import           HGeometry.Boundary
import           HGeometry.Kernel
import           HGeometry.Kernel.Instances ()
import           HGeometry.Polygon.Simple
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           R
import           HGeometry.Sign

--------------------------------------------------------------------------------

spec :: Spec
spec = do it "Pick point in polygon test"  $
            toSpec testPoly
          it "Pick point in polygon test"  $
            toSpec testPoly2

          prop "halfspace switching lineeq -> linePV is consistent" $
            \(h@(HalfSpace s l) :: HalfSpaceF (LineEQ R)) (q :: Point 2 R) ->
              let h' = HalfSpace s (supportingLine l)
              in counterexample (show h') $ q `intersects` h === q `intersects` h'

          prop "halfspace switching VerticalOrLineEQ -> linePV is consistent" $
            \(h@(HalfSpace s l) :: HalfSpaceF (VerticalOrLineEQ R)) (q :: Point 2 R) ->
              let s' = case l of
                         VerticalLineThrough _ -> flipSign s
                         NonVertical _         -> s
                  h' = HalfSpace s' (supportingLine l)
              in counterexample (show h') $ q `intersects` h === q `intersects` h'

          prop "pick point inside halfplane correct (LinePV)" $
            \(h :: HalfSpaceF (LinePV 2 R)) ->
              pointInteriorTo h `intersects` h
          prop "pick point inside halfplane correct (LineEQ)" $
            \(h :: HalfSpaceF (LineEQ R)) ->
              pointInteriorTo h `intersects` h
          prop "pick point inside halfplane correct (VerticalOrLineEQ)" $
            \(h :: HalfSpaceF (VerticalOrLineEQ R)) ->
              pointInteriorTo h `intersects` h


data TestCase r = TestCase { _polygon :: SimplePolygon (Point 2 r) }
                  deriving (Show)

toSpec                  :: TestCase R -> Expectation
toSpec (TestCase  poly) = ((centroid poly :: Point 2 R)
                           `inPolygon` poly) `shouldBe` StrictlyInside

testPoly :: TestCase R
testPoly = TestCase . fromJust . fromPoints . NonEmpty.fromList
         $ [origin, Point2 10 10, Point2 20 5, Point2 30 2, Point2 3 1, Point2 31 0]

testPoly2 :: TestCase R
testPoly2 = TestCase . fromJust . fromPoints  . NonEmpty.fromList
          $ [ Point2 208 752
            , Point2 304 688
            , Point2 224 592
            , Point2 48 736
            ]
