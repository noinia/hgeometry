module HalfPlane.Cone.IntersectRect
  ( toConvexPolygonIn
  , extraPoints
  ) where

import HGeometry.Cone
import HGeometry.Point
import HGeometry.Line
import HGeometry.Vector
import HGeometry.Ext
import HGeometry.Box
import HGeometry.HalfLine
import HGeometry.Polygon
import HGeometry.Direction
import HGeometry.Intersection
import HGeometry.Properties
import HGeometry.LineSegment
import Data.Monoid
import Control.Lens
import Data.List.NonEmpty (NonEmpty(..))
import Data.List.NonEmpty qualified as NonEmpty
import HGeometry.Cyclic
import HGeometry.Point
import HGeometry.Point.Either

import Debug.Trace
--------------------------------------------------------------------------------


-- | turn the cone into a convex polygon by clipping it in the given rectangle.
--
-- pre:  the apex lies inside the given rectangle
toConvexPolygonIn        :: ( Rectangle_ rectangle point
                            , Point_ point 2 r, Ord r, Fractional r
                            -- , IsIntersectableWith (HalfLine point) (ClosedLineSegment point)
                            -- , Intersection (HalfLine point) (ClosedLineSegment point)
                            --   ~ Maybe (HalfLineLineSegmentIntersection (Point 2 r)
                            --                                          (ClosedLineSegment point))
                            )
                         => rectangle
                         -> Cone r point edge
                         -> ConvexPolygonF (Cyclic NonEmpty) (OriginalOrExtra point (Point 2 r))
toConvexPolygonIn rect c = let hp     = (leftBoundary  c ^.core)&halfLineStart %~ (^.asPoint)
                               hq     = (rightBoundary c ^.core)&halfLineStart %~ (^.asPoint)
                               extras = extraPoints hp hq $ Box (rect^.minPoint.asPoint)
                                                                (rect^.maxPoint.asPoint)
                           in uncheckedFromCCWPoints $
                                Original (c^.apex) NonEmpty.<| (Extra <$> extras)


-- | computes the extra vertices that we have to insert to make an unbounded region bounded
extraPoints            :: ( Rectangle_ rectangle corner, Point_ corner 2 r
                          , Point_ point 2 r, Fractional r, Ord r
                          , IsIntersectableWith (HalfLine point) (ClosedLineSegment corner)
                          , Intersection (HalfLine point) (ClosedLineSegment corner)
                            ~ Maybe (HalfLineLineSegmentIntersection (Point 2 r)
                                                                     (ClosedLineSegment corner))
                          )
                       => HalfLine point -> HalfLine point -> rectangle
                       -> NonEmpty (Point 2 r)
extraPoints hp hq rect = noDuplicates $ q :| cornersInBetween qSide pSide rect <> [p]
    -- if the intersection point coincides with a corner then the current code includes
    -- the corner. We use the noDuplicates to get rid of those.
  where
    (q,qSide) = intersectionPoint hq
    (p,pSide) = intersectionPoint hp

    intersectionPoint  h = case getFirst $ intersectionPoint' h of
                             Nothing -> error "extraPoints: precondititon failed "
                             Just x  -> x
    intersectionPoint' h = flip ifoldMap (sides rect) $ \side seg ->
      case h `intersect` seg of
        Just (HalfLine_x_LineSegment_Point x) -> First $ Just (x, side)
        _                                     -> First   Nothing

    noDuplicates = fmap NonEmpty.head . NonEmpty.group1


-- | Computes the corners in between the two given sides (in CCW order)
cornersInBetween          :: (Rectangle_ rectangle point, Point_ point 2 r, Num r)
                          => CardinalDirection -> CardinalDirection -> rectangle -> [Point 2 r]
cornersInBetween s e rect = map snd
                          . takeWhile ((/= e) . fst) . dropWhile ((/= s) . fst)
                          $ cycle [(East,tr),(North,tl),(West,bl),(South,br)]
  where
    Corners tl tr br bl = view asPoint <$> corners rect
