module Algorithms.Geometry.PolygonTriangulation.MakeMonotoneSpec where

import Algorithms.Geometry.PolygonTriangulation.MakeMonotone
import Data.Geometry.Polygon
import Data.Geometry
import Data.Ext
import Control.Lens
import Test.Hspec
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Set as Set
-- import Data.Geometry.Ipe


spec :: Spec
spec = describe "GeomBook Example" $ do
         it "Classify Verticese" $
           (fmap (^.extra.extra) . polygonVertices $ classifyVertices geomBookPoly)
           `shouldBe` geomBookVertexTypes
         it "Diagonals" $
           (Set.fromList . map (\s -> sort' (s^.start.extra,s^.end.extra))
            $ computeDiagonals geomBookPoly)
           `shouldBe` geomBookDiagonals


  -- testCases "test/Algorithms/Geometry/SmallestEnclosingDisk/manual.ipe"

sort'       :: Ord a => (a,a) -> (a,a)
sort' (x,y) = (min x y, max x y)

geomBookPoly :: SimplePolygon Int Rational
geomBookPoly = fromPoints [ Point2 20 20 :+ 1
                          , Point2 18 19 :+ 2
                          , Point2 16 25 :+ 3
                          , Point2 13 23 :+ 4
                          , Point2 10 24 :+ 5
                          , Point2 6  22 :+ 6
                          , Point2 8  21 :+ 7
                          , Point2 7  18 :+ 8
                          , Point2 2  19 :+ 9
                          , Point2 1  10 :+ 10
                          , Point2 3  5  :+ 11
                          , Point2 11 7  :+ 12
                          , Point2 15 1  :+ 13
                          , Point2 12 15 :+ 14
                          , Point2 15 12 :+ 15
                          ]
geomBookVertexTypes = NonEmpty.fromList [Start,Merge,Start,Merge,Start,Regular,Regular,Merge,Start,Regular,End,Split,End,Split,End]
geomBookDiagonals = Set.fromList [(4,6),(2,8),(8,14),(10,12)]



  -- let f i     = geomBookPoly !! (i-1)
  --                       seg i j =
  --                   in

  -- [Clo

  -- LineSegment (Closed (Point2 [6 % 1,22 % 1] :+ 6)) (Closed (Point2 [13 % 1,23 % 1] :+ 4)),LineSegment (Closed (Point2 [7 % 1,18 % 1] :+ 8)) (Closed (Point2 [18 % 1,19 % 1] :+ 2)),LineSegment (Closed (Point2 [12 % 1,15 % 1] :+ 14)) (Closed (Point2 [7 % 1,18 % 1] :+ 8)),LineSegment (Closed (Point2 [11 % 1,7 % 1] :+ 12)) (Closed (Point2 [1 % 1,10 % 1] :+ 10))]
