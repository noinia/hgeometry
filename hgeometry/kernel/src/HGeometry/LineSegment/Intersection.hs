{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module HGeometry.LineSegment.Intersection
  ( LineLineSegmentIntersection(..)
  , LineSegmentLineSegmentIntersection(..)
  ) where

import Control.Lens
import HGeometry.HyperPlane.Class
import HGeometry.Intersection
import HGeometry.Interval.EndPoint
import HGeometry.Line
import HGeometry.LineSegment.Internal
import HGeometry.Point
import HGeometry.Properties (NumType)

--------------------------------------------------------------------------------
-- * Line x LineSegment Intersection

-- | Line x LineSegment intersection
data LineLineSegmentIntersection lineSegment =
    Line_x_LineSegment_Point       (Point 2 (NumType lineSegment))
  | Line_x_LineSegment_LineSegment lineSegment

deriving instance (Show (Point 2 (NumType lineSegment)), Show lineSegment
                  ) => Show (LineLineSegmentIntersection lineSegment)
deriving instance (Eq (Point 2 (NumType lineSegment)), Eq lineSegment
                  ) => Eq (LineLineSegmentIntersection lineSegment)

type Intersection' lineSegment = Maybe (LineLineSegmentIntersection lineSegment)

type instance Intersection (LinePV 2 r) (LineSegment (EndPoint t) point) =
  Intersection' (LineSegment (EndPoint t) point)

type instance Intersection (LineEQ r) (LineSegment (EndPoint t) point) =
  Intersection' (LineSegment (EndPoint t) point)

type instance Intersection (LinePV 2 r) (LineSegment AnEndPoint point) =
  Intersection' (LineSegment AnEndPoint point)

type instance Intersection (LineEQ r) (LineSegment AnEndPoint point) =
  Intersection' (LineSegment AnEndPoint point)

----------------------------------------
-- * HasIntersectionWith

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

instance ( Point_ point 2 r, Num r,  Ord r
         ) => LineEQ r `HasIntersectionWith` LineSegment AnEndPoint point where
  intersects = intersectsImpl
  {-# INLINE intersects #-}
instance ( Point_ point 2 r, Num r,  Ord r
         ) => LinePV 2 r `HasIntersectionWith` LineSegment AnEndPoint point where
  intersects = intersectsImpl
  {-# INLINE intersects #-}


-- | Test whether a line in R^2 intersects a closed linesegment
intersectsImpl       :: ( HyperPlane_ line 2 r
                        , Point_ point 2 r
                        , Num r, Ord r
                        , LineSegment_ lineSegment point
                        ) => line -> lineSegment -> Bool
l `intersectsImpl` s = case (onSideTest (s^.start) l, onSideTest (s^.end) l) of
                         (LT, LT) -> False
                         (LT, EQ) -> s^.endPoint.to endPointType == Closed
                         (LT, GT) -> True
                         (EQ, EQ) -> True
                         (EQ, _)  -> s^.startPoint.to endPointType == Closed
                         (GT, LT) -> True
                         (GT, EQ) -> s^.endPoint.to endPointType == Closed
                         (GT, GT) -> False
  -- case onSideTest (s^.start) l of
  --                        EQ   -> True
  --                        side -> side /= onSideTest (s^.end) l
{-# INLINE intersectsImpl #-}


----------------------------------------
-- * IsIntersectableWith

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

instance ( Point_ point 2 r
         , Fractional r,  Ord r
         ) => LinePV 2 r `IsIntersectableWith` LineSegment AnEndPoint point where
  intersect = intersectImpl
  {-# INLINE intersect #-}

instance ( Point_ point 2 r
         , Fractional r,  Ord r
         ) => LineEQ r `IsIntersectableWith` LineSegment AnEndPoint point where
  intersect = intersectImpl
  {-# INLINE intersect #-}

-- | Implementation for intersects between lines and line segments.
--
-- the type is is sufficiently general that for various line or closed line segment types
-- we can appeal to it.
intersectImpl       :: ( HyperPlane_ line 2 r
                       , Point_ point 2 r
                       , Fractional r, Ord r
                       , LineSegment_ lineSegment point
                       , HasSupportingLine lineSegment
                       , HasOnSegment lineSegment 2
                       , line `IsIntersectableWith` LinePV 2 r
                       , line `HasIntersectionWith` lineSegment
                       , Intersection line (LinePV 2 r) ~ Maybe (LineLineIntersection line)
                       ) => line -> lineSegment
                    -> Maybe (LineLineSegmentIntersection lineSegment)
l `intersectImpl` s = l `intersect` supportingLine s >>= \case
    Line_x_Line_Point p | p `onSegment` s -> Just $ Line_x_LineSegment_Point p
                        | otherwise       -> Nothing
    Line_x_Line_Line _  -> Just $ Line_x_LineSegment_LineSegment s
{-# INLINE intersectImpl #-}

--------------------------------------------------------------------------------
-- * LineSegment x LineSegment

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

type instance Intersection (LineSegment AnEndPoint point)
                           (LineSegment AnEndPoint point) =
  Maybe (LineSegmentLineSegmentIntersection (LineSegment AnEndPoint point))

----------------------------------------
-- * HasIntersectionWith

instance ( Point_ point 2 r, Num r,  Ord r
         ) =>
         LineSegment AnEndPoint point `HasIntersectionWith` LineSegment AnEndPoint point where
  s `intersects` s' = supportingLine s `intersects` s' && supportingLine s' `intersects` s
  {-# INLINE intersects #-}

----------------------------------------
-- * IsIntersectableWith

instance ( Point_ point 2 r, Fractional r,  Ord r
         ) =>
         LineSegment AnEndPoint point `IsIntersectableWith` LineSegment AnEndPoint point where
  intersect s s' = intersect (supportingLine s) s' >>= \case
    Line_x_LineSegment_Point p
      | p `onSegment` s              -> Just $ LineSegment_x_LineSegment_Point p
      | otherwise                    -> Nothing
    Line_x_LineSegment_LineSegment _ -> Just $ LineSegment_x_LineSegment_LineSegment undefined
  {-# INLINE intersect #-}


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
