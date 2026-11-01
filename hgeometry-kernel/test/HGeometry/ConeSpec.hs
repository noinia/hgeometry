module HGeometry.ConeSpec where

import Control.Lens
import Test.Hspec
import HGeometry.Kernel
import HGeometry.Cone
import HGeometry.Ext
import HGeometry.HalfLine
import R

--------------------------------------------------------------------------------


spec :: Spec
spec = it "cone triangle boundary intersections" $
  let testT :: Triangle (Point 2 R)
      testT =  Triangle (Point2 1 0) (Point2 1 1) (Point2 0 0)

      testC :: Cone R (Point 2 R) ()
      testC = Cone (Point2 1 0) (Vector2 1 1 :+ ()) (Vector2 0 (-1) :+ ())
  in coneTriangleBoundaryIntersections testC testT
     `shouldBe`
     [Point2 1 0,Point2 1 0,Point2 1 0,Point2 1 0]


-- | Computes the intersection points of the boundary of a cone with
-- the boundary of a triangle.
--
-- this may contain duplicates.
coneTriangleBoundaryIntersections :: forall apex corner r edge.
                                     ( Point_ apex 2 r, Point_ corner 2 r
                                     , Ord r, Fractional r
                                     )
                                  => Cone r apex edge -> Triangle corner
                                  -> [Point 2 r]
coneTriangleBoundaryIntersections cone (fmap (^.asPoint) -> Triangle a b c) =
    intersectionPoints (leftBoundary cone) <> intersectionPoints (rightBoundary cone)
  where
    intersectionPoints (HalfLine o v :+ _) =
      flip foldMap sides $ \side -> case HalfLine (o^.asPoint) v `intersect` side of
        Nothing                                       -> []
        Just (HalfLine_x_LineSegment_Point p)         -> [p]
        Just (HalfLine_x_LineSegment_LineSegment seg) -> [seg^.start,seg^.end]

    sides = [ ClosedLineSegment a b
            , ClosedLineSegment b c
            , ClosedLineSegment c a
            ]
