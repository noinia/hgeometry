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
geomBookPoly = fromPoints [ point2 20 20 :+ 1
                          , point2 18 19 :+ 2
                          , point2 16 25 :+ 3
                          , point2 13 23 :+ 4
                          , point2 10 24 :+ 5
                          , point2 6  22 :+ 6
                          , point2 8  21 :+ 7
                          , point2 7  18 :+ 8
                          , point2 2  19 :+ 9
                          , point2 1  10 :+ 10
                          , point2 3  5  :+ 11
                          , point2 11 7  :+ 12
                          , point2 15 1  :+ 13
                          , point2 12 15 :+ 14
                          , point2 15 12 :+ 15
                          ]
geomBookVertexTypes :: NonEmpty.NonEmpty VertexType
geomBookVertexTypes = NonEmpty.fromList [Start,Merge,Start,Merge,Start,Regular,Regular,Merge,Start,Regular,End,Split,End,Split,End]
geomBookDiagonals :: Set.Set (Int, Int)
geomBookDiagonals = Set.fromList [(4,6),(2,8),(8,14),(10,12)]



  -- let f i     = geomBookPoly !! (i-1)
  --                       seg i j =
  --                   in

  -- [Clo

  -- LineSegment (Closed (Point2 [6 % 1,22 % 1] :+ 6)) (Closed (Point2 [13 % 1,23 % 1] :+ 4)),LineSegment (Closed (Point2 [7 % 1,18 % 1] :+ 8)) (Closed (Point2 [18 % 1,19 % 1] :+ 2)),LineSegment (Closed (Point2 [12 % 1,15 % 1] :+ 14)) (Closed (Point2 [7 % 1,18 % 1] :+ 8)),LineSegment (Closed (Point2 [11 % 1,7 % 1] :+ 12)) (Closed (Point2 [1 % 1,10 % 1] :+ 10))]
