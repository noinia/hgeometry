module Polygon.Triangulation.MakeMonotoneSpec where

import           Control.Lens
import           Data.Maybe (fromJust)
import qualified Data.Set as Set
import           HGeometry.Ext
import           HGeometry.Point
import           HGeometry.LineSegment
import           HGeometry.Polygon.Class
import           HGeometry.Polygon.Simple
import           HGeometry.Polygon.Triangulation.MakeMonotone
import           HGeometry.Vector
import           Test.Hspec

--------------------------------------------------------------------------------

spec :: Spec
spec = describe "GeomBook Example" $ do
         it "Classify Vertices" $
           classifyVertices geomBookPoly
           `shouldBe`
           (over _2 (^.extra._2) <$> geomBookPoly^@..vertices)

         it "Diagonals" $
           (Set.fromList . map (sort' . lookupNum geomBookPoly) $ computeDiagonals geomBookPoly)
           `shouldBe` geomBookDiagonals

         it "cmpXtest" $ do
           let q = Point2 11 7
               seg = ClosedLineSegment (Point2 1 10) (Point2 3 5)
           cmpX q seg `shouldBe` LT

         it "cmpXtest2" $ do
           let q = Point2 13 23
               seg1 = ClosedLineSegment (Point2 10 24) (Point2 6  22)
           cmpX q seg1 `shouldBe` LT
         it "cmpXtest3" $ do
           let q = Point2 13 23
               seg2 = ClosedLineSegment (Point2 16 25) (Point2 13 23)
           cmpX q seg2 `shouldBe` EQ

sort'               :: Ord a => Vector 2 a-> Vector 2 a
sort' (Vector2 x y) = Vector2 (min x y) (max x y)

lookupNum    :: SimplePolygon (point :+ (i,e)) -> Vector 2 Int -> Vector 2 i
lookupNum pg = fmap (\i -> pg^?!vertexAt i.extra._1)

geomBookPoly :: SimplePolygon (Point 2 Int :+ (Int, VertexType))
geomBookPoly = fromJust
             $ fromPoints [ Point2 20 20 :+ (1 , Start   )
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

geomBookDiagonals = Set.fromList [Vector2 4  6
                                 ,Vector2 2  8
                                 ,Vector2 8  14
                                 ,Vector2 10 12
                                 ]
