module Data.Geometry.LineSpec where

import Data.Ext
import Control.Lens
import Data.Geometry
import Data.Geometry.Box
import Data.Vinyl.CoRec
import Test.Hspec
import Data.Ratio




spec :: Spec
spec = do
  describe "Line x Box intersections" $ do
    boxIntersections

boxIntersections :: Spec
boxIntersections = do
    it "proper intersection" $
      (lineThrough (Point2 1 5) (Point2 10 (7 :: Rational))
       `intersect` b
      ) `shouldBe`
      (coRec $ ClosedLineSegment (ext $ Point2 (0 :: Rational) (43 % 9))
                                 (ext $ Point2 14              (71 % 9))
      )
    it "boundary segment" $
      (lineThrough (Point2 0 0) (Point2 10 (0 :: Rational))
       `intersect` b
      ) `shouldBe`
      (coRec $ ClosedLineSegment (ext $ Point2 (0 :: Rational) 0)
                                 (ext $ Point2 14              0)
      )
    it "Touching in Point" $
      (lineThrough (Point2 0 0) (Point2 (-1) (1 :: Rational))
       `intersect`
       boundingBoxList' [Point2 0 (0 :: Rational), Point2 14 9]
      ) `shouldBe`
      (coRec (origin :: Point 2 Rational))
    it "No Intersection" $
      (lineThrough (Point2 (-1) 0) (Point2 (-2) (2 :: Rational))
       `intersect`
       boundingBoxList' [Point2 0 (0 :: Rational), Point2 14 9]
      ) `shouldBe`
      (coRec NoIntersection)
  where
    b = boundingBoxList' [Point2 0 (0 :: Rational), Point2 14 9]
