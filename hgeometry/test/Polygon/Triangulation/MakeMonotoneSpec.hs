module Polygon.Triangulation.MakeMonotoneSpec where

import           Control.Lens
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Set as Set
import           HGeometry.Ext
import           HGeometry.Point
import           HGeometry.Polygon.Class
import           HGeometry.Polygon.Simple
import           HGeometry.Polygon.Triangulation.MakeMonotone
import           HGeometry.Vector
import           Test.Hspec

--------------------------------------------------------------------------------

spec :: Spec
spec = describe "GeomBook Example" $ do
         it "Classify Verticese" $
           classifyVertices geomBookPoly
           `shouldBe`
           (over _2 (view extra._2) <$> geomBookPoly^@..vertices)

         it "Diagonals" $
           (Set.fromList . map (sort' . lookupNum geomBookPoly) $ computeDiagonals geomBookPoly)
           `shouldBe` geomBookDiagonals




  -- testCases "test/Algorithms/Geometry/SmallestEnclosingDisk/manual.ipe"

sort'               :: Ord a => Vector 2 a-> Vector 2 a
sort' (Vector2 x y) = Vector2 (min x y) (max x y)

lookupNum    :: SimplePolygon (point :+ (i,e)) -> Vector 2 Int -> Vector 2 i
lookupNum pg = fmap (\i -> pg^@!vertexAt i.extra._1)

geomBookPoly :: SimplePolygon (Point 2 Int :+ (Int, VertexType))
geomBookPoly = fromPoints [ Point2 20 20 :+ (1 , Start   )
                          , Point2 18 19 :+ (2 , Merge   )
                          , Point2 16 25 :+ (3 , Start   )
                          , Point2 13 23 :+ (4 , Merge   )
                          , Point2 10 24 :+ (5 , Start   )
                          , Point2 6  22 :+ (6 , Regular )
                          , Point2 8  21 :+ (7 , Regular )
                          , Point2 7  18 :+ (8 , Merge   )
                          , Point2 2  19 :+ (9 , Start   )
                          , Point2 1  10 :+ (10, Regular )
                          , Point2 3  5  :+ (11, End     )
                          , Point2 11 7  :+ (12, Split   )
                          , Point2 15 1  :+ (13, End     )
                          , Point2 12 15 :+ (14, Split   )
                          , Point2 15 12 :+ (15, End     )
                          ]

geomBookDiagonals = Set.fromList [(4,6),(2,8),(8,14),(10,12)]



  -- let f i     = geomBookPoly !! (i-1)
  --                       seg i j =
  --                   in

  -- [Clo

  -- LineSegment (Closed (Point2 [6 % 1,22 % 1] :+ 6)) (Closed (Point2 [13 % 1,23 % 1] :+ 4)),LineSegment (Closed (Point2 [7 % 1,18 % 1] :+ 8)) (Closed (Point2 [18 % 1,19 % 1] :+ 2)),LineSegment (Closed (Point2 [12 % 1,15 % 1] :+ 14)) (Closed (Point2 [7 % 1,18 % 1] :+ 8)),LineSegment (Closed (Point2 [11 % 1,7 % 1] :+ 12)) (Closed (Point2 [1 % 1,10 % 1] :+ 10))]
