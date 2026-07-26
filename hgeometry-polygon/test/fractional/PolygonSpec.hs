module PolygonSpec
  ( spec
  ) where

import qualified Data.List.NonEmpty as NonEmpty
import           Control.Lens
import           HGeometry.Polygon
import           HGeometry.Point
import           Test.Hspec
import           R

--------------------------------------------------------------------------------

spec :: Spec
spec = describe "polygon tests" $ do
         it "outerBoundaryEdges" $ do
           let pg :: SimplePolygon (Point 2 R)
               pg = uncheckedFromCCWPoints $ NonEmpty.fromList
                     [ Point2 0 0 :: Point 2 R
                     , Point2 1 0
                     , Point2 0 1
                     ]
           (pg^..outerBoundaryEdges.withIndex) `shouldBe`
              [((0,1),(Point2 0 0,Point2 1 0))
              ,((1,2),(Point2 1 0,Point2 0 1))
              ,((2,0),(Point2 0 1,Point2 0 0))
              ]
         describe "fromPoints; simple polygon test" $ do
           let pts = NonEmpty.fromList [ Point2 1 (-1)
                                       , Point2 1 (-2)
                                       , Point2 0 0
                                       , Point2 0 0 :: Point 2 R
                                       ]
           it "fromPoints does not crash on this input" $
             (fromPoints pts :: Maybe (SimplePolygon (Point 2 R))) `shouldBe`
             Just (uncheckedFromCCWPoints . NonEmpty.fromList $
               [Point2 0 0,Point2 1 (-2),Point2 1 (-1)])
           it "fromPoints does not crash on this input" $
             (fromPoints pts :: Maybe (ConvexPolygon (Point 2 R))) `shouldBe`
             Just (uncheckedFromCCWPoints . NonEmpty.fromList $
               [Point2 0 0,Point2 1 (-2),Point2 1 (-1)])
