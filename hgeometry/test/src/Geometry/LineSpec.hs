module Geometry.LineSpec where

import Data.Ext
import Data.Intersection
import Data.Ratio
import Geometry.Box
import Geometry.Line
import Geometry.LineSegment
import Geometry.Point
import Test.Hspec




spec :: Spec
spec = do
  describe "Line x Box intersections" $ do
    boxIntersections

point2 :: r -> r -> Point 2 r
point2 = Point2

boxIntersections :: Spec
boxIntersections = do
    it "proper intersection" $
      (lineThrough @Line (point2 1 5) (point2 10 (7 :: Rational))
       `intersect` b
      ) `shouldBe`
      (coRec $ ClosedLineSegment (ext $ point2 (0 :: Rational) (43 % 9))
                                 (ext $ point2 14              (71 % 9))
      )
    it "boundary segment" $
      (lineThrough @Line (point2 0 0) (point2 10 (0 :: Rational))
       `intersect` b
      ) `shouldBe`
      (coRec $ ClosedLineSegment (ext $ point2 (0 :: Rational) 0)
                                 (ext $ point2 14              0)
      )
    it "Touching in Point" $
      (lineThrough @Line (point2 0 0) (point2 (-1) (1 :: Rational))
       `intersect`
       boundingBoxList' [point2 0 (0 :: Rational), point2 14 9]
      ) `shouldBe`
      (coRec (origin :: Point 2 Rational))
    it "No Intersection" $
      (lineThrough @Line (point2 (-1) 0) (point2 (-2) (2 :: Rational))
       `intersect`
       boundingBoxList' [point2 0 (0 :: Rational), point2 14 9]
      ) `shouldBe`
      (coRec NoIntersection)
  where
    b = boundingBoxList' [point2 0 (0 :: Rational), point2 14 9]
