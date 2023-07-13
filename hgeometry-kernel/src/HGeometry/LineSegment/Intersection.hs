{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module HGeometry.LineSegment.Intersection
  (

  ) where

import Control.Applicative
import Control.Lens
import HGeometry.HyperPlane.Class
import HGeometry.Intersection
import HGeometry.Line
import HGeometry.LineSegment.Internal
import HGeometry.Point
import HGeometry.Properties (NumType)

--------------------------------------------------------------------------------

-- | Line x LineSegment intersection
data LineLineSegmentIntersection line lineSegment =
    Line_x_LineSegment_Point       (Point 2 (NumType lineSegment))
  | Line_x_LineSegment_LineSegment lineSegment

deriving instance (Show (Point 2 (NumType lineSegment)), Show lineSegment
                  ) => Show (LineLineSegmentIntersection line lineSegment)
deriving instance (Eq (Point 2 (NumType lineSegment)), Eq lineSegment
                  ) => Eq (LineLineSegmentIntersection line lineSegment)


type instance Intersection (LinePV 2 r) (LineSegment (EndPoint t) point) =
  Maybe (LineLineSegmentIntersection (LinePV 2 r) (LineSegment (EndPoint t) point))

type instance Intersection (LineEQ r) (LineSegment (EndPoint t) point) =
  Maybe (LineLineSegmentIntersection (LineEQ r) (LineSegment (EndPoint t) point))


instance ( Point_ point 2 r, Num r, Ord r
         ) => LinePV 2 r `HasIntersectionWith` ClosedLineSegment point where
  intersects = intersectsImpl
  {-# INLINE intersects #-}

instance ( Point_ point 2 r, Num r, Ord r
         ) => LineEQ r `HasIntersectionWith` ClosedLineSegment point where
  --
  -- >>> LineEQ 1 2 `intersects` ClosedLineSegment origin (Point2 1 10)
  -- True
  intersects = intersectsImpl
  {-# INLINE intersects #-}

-- | Test whether a line in R^2 intersects a closed linesegment
intersectsImpl       :: ( HyperPlane_ line 2 r
                       , Point_ point 2 r
                       , Num r, Ord r
                       , ClosedLineSegment_ lineSegment point
                       ) => line -> lineSegment -> Bool
l `intersectsImpl` s = case onSideTest (s^.start) l of
                         EQ   -> True
                         side -> side /= onSideTest (s^.end) l
{-# INLINE intersectsImpl #-}

instance ( Point_ point 2 r
         , Fractional r,  Ord r
         ) => LinePV 2 r `IsIntersectableWith` ClosedLineSegment point where
  intersect = intersectImpl
  {-# INLINE intersect #-}

instance ( Point_ point 2 r
         , Fractional r,  Ord r
         ) => LineEQ r `IsIntersectableWith` ClosedLineSegment point where
  intersect = intersectImpl
  {-# INLINE intersect #-}

-- | Implementation for intersects between lines and line segments.
--
-- the type is is sufficiently general that for various line or closed line segment types
-- we can appeal to it.
intersectImpl       :: ( HyperPlane_ line 2 r
                       , Point_ point 2 r
                       , Fractional r, Ord r
                       , ClosedLineSegment_ lineSegment point
                       , HasSupportingLine lineSegment
                       , line `IsIntersectableWith` LinePV 2 r
                       , line `HasIntersectionWith` lineSegment
                       , Intersection line (LinePV 2 r) ~ Maybe (LineLineIntersection line)
                       ) => line -> lineSegment
                    -> Maybe (LineLineSegmentIntersection line lineSegment)
l `intersectImpl` s
  | l `intersects` s = l `intersect` supportingLine s <&> \case
                            Line_x_Line_Point p -> Line_x_LineSegment_Point p
                            Line_x_Line_Line _  -> Line_x_LineSegment_LineSegment s
  | otherwise        = empty
{-# INLINE intersectImpl #-}

--------------------------------------------------------------------------------

-- | LineSegment x LineSegment intersection
data LineSegmentLineSegmentIntersection lineSegment =
    LineSegment_x_LineSegment_Point       (Point 2 (NumType lineSegment))
  | LineSegment_x_LineSegment_LineSegment lineSegment

deriving instance (Show (Point 2 (NumType lineSegment)), Show lineSegment
                  ) => Show (LineSegmentLineSegmentIntersection lineSegment)
deriving instance (Eq (Point 2 (NumType lineSegment)), Eq lineSegment
                  ) => Eq (LineSegmentLineSegmentIntersection lineSegment)

type instance Intersection (LineSegment (EndPoint t) point)
                           (LineSegment (EndPoint t) point) =
  Maybe (LineSegmentLineSegmentIntersection (LineSegment (EndPoint t) point))

{-

instance ( Point_ point 2 r, Num r, Ord r
         ) => ClosedLineSegment point `HasIntersectionWith` ClosedLineSegment point where
  sa `intersects` sb = supportingLine sa `intersects` sb &&
                       supportingLine sb `intersects` sa
  {-# INLINE intersects #-}
  -- FIXME: this is not correct yet; i.e. if sa and sb are colinear both supportinging
  -- lines intersect the segment, but the segments may stil lbe disjoint.


instance ( Point_ point 2 r
         , Fractional r,  Ord r
         ) => ClosedLineSegment point `IsIntersectableWith` ClosedLineSegment point where
  sa `intersect` sb
    | sa `intersects` sb = Just undefined
      -- FIXME!! continue here
    | otherwise          = Nothing
  {-# INLINE intersect #-}

-}
