{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module HGeometry.LineSegment.Intersection
  ( LineLineSegmentIntersection(..)
  , LineSegmentLineSegmentIntersection(..)
  ) where

import Control.Lens
import HGeometry.Ext
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
         , IxValue (endPoint point) ~ point
         , EndPoint_ (endPoint point)
         ) => LinePV 2 r `HasIntersectionWith` LineSegment endPoint point where
  intersects = intersectsImpl
  {-# INLINE intersects #-}

instance ( Point_ point 2 r, Num r, Ord r
         , IxValue (endPoint point) ~ point
         , EndPoint_ (endPoint point)
         ) => LineEQ r `HasIntersectionWith` LineSegment endPoint point where
  --
  -- >>> LineEQ 1 2 `intersects` ClosedLineSegment origin (Point2 1 10)
  -- True
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

instance ( Point_ point 2 r
         , Fractional r,  Ord r
         ) => LinePV 2 r `IsIntersectableWith` OpenLineSegment point where
  intersect = intersectImpl
  {-# INLINE intersect #-}

instance ( Point_ point 2 r
         , Fractional r,  Ord r
         ) => LineEQ r `IsIntersectableWith` OpenLineSegment point where
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


-- | fmap' for lineSegmentIntersections
fmap'   :: (NumType lineSegment ~ NumType lineSegment')
        => (lineSegment -> lineSegment')
        -> LineSegmentLineSegmentIntersection lineSegment
        -> LineSegmentLineSegmentIntersection lineSegment'
fmap' f = \case
  LineSegment_x_LineSegment_Point p       -> LineSegment_x_LineSegment_Point p
  LineSegment_x_LineSegment_LineSegment s -> LineSegment_x_LineSegment_LineSegment (f s)

----------------------------------------
-- * HasIntersectionWith

instance ( Point_ point 2 r, Num r,  Ord r
         , IxValue (endPoint point) ~ point
         , EndPoint_ (endPoint point)
         ) =>
         LineSegment endPoint point `HasIntersectionWith` LineSegment endPoint point where
  s `intersects `s' = supportingLine s `intersects` s' && supportingLine s' `intersects` s
  {-# INLINE intersects #-}
  -- this does not really work; i.e. if the segments are colinear

instance ( Point_ point 2 r, Num r,  Ord r
         ) =>
         LineSegment AnEndPoint point `HasIntersectionWith` ClosedLineSegment point where
  s `intersects `s' = supportingLine s `intersects` s' && supportingLine s' `intersects` s
  {-# INLINE intersects #-}

instance ( Point_ point 2 r, Num r,  Ord r
         ) =>
         LineSegment AnEndPoint point `HasIntersectionWith` OpenLineSegment point where
  s `intersects `s' = supportingLine s `intersects` s' && supportingLine s' `intersects` s
  {-# INLINE intersects #-}

instance ( Point_ point 2 r, Num r,  Ord r
         ) =>
         ClosedLineSegment point `HasIntersectionWith` LineSegment AnEndPoint point where
  s `intersects `s' = supportingLine s `intersects` s' && supportingLine s' `intersects` s
  {-# INLINE intersects #-}

instance ( Point_ point 2 r, Num r,  Ord r
         ) =>
         ClosedLineSegment point `HasIntersectionWith` OpenLineSegment point where
  s `intersects `s' = supportingLine s `intersects` s' && supportingLine s' `intersects` s
  {-# INLINE intersects #-}

instance ( Point_ point 2 r, Num r,  Ord r
         ) =>
         OpenLineSegment point `HasIntersectionWith` LineSegment AnEndPoint point where
  s `intersects `s' = supportingLine s `intersects` s' && supportingLine s' `intersects` s
  {-# INLINE intersects #-}

instance ( Point_ point 2 r, Num r,  Ord r
         ) =>
         OpenLineSegment point `HasIntersectionWith` ClosedLineSegment point where
  s `intersects `s' = supportingLine s `intersects` s' && supportingLine s' `intersects` s
  {-# INLINE intersects #-}

----------------------------------------
-- * IsIntersectableWith

-- instance ( Point_ point 2 r, Fractional r,  Ord r
--          ) =>
--          LineSegment AnEndPoint point `IsIntersectableWith` LineSegment AnEndPoint point where
--   s `intersect` s' = supportingLine s `intersect` s' >>= \case
--     Line_x_LineSegment_Point p
--       | p `onSegment` s              -> Just $ LineSegment_x_LineSegment_Point p
--       | otherwise                    -> Nothing
--     Line_x_LineSegment_LineSegment _ -> Just $ LineSegment_x_LineSegment_LineSegment todo
--       where
--         todo = error "LineSegment_x_LineSegment_LineSegment, not yet implemented"
--   {-# INLINE intersect #-}

instance ( Point_ point 2 r, Fractional r,  Ord r
         , IxValue (endPoint point) ~ point
         , EndPoint_ (endPoint point)
         , IsIntersectableWith (LinePV 2 r) (LineSegment endPoint point)
         , HasOnSegment (LineSegment endPoint point) 2
         , Intersection (LineSegment endPoint point) (LineSegment endPoint point)
           ~ Maybe (LineSegmentLineSegmentIntersection (LineSegment endPoint point))
         , Intersection (LinePV 2 r) (LineSegment endPoint point)
           ~ Maybe (LineLineSegmentIntersection (LineSegment endPoint point))
         ) =>
         LineSegment endPoint point `IsIntersectableWith` LineSegment endPoint point where
  s `intersect` s' = supportingLine s `intersect` s' >>= \case
    Line_x_LineSegment_Point p
      | p `onSegment` s              -> Just $ LineSegment_x_LineSegment_Point p
      | otherwise                    -> Nothing
    Line_x_LineSegment_LineSegment _ -> Just $ LineSegment_x_LineSegment_LineSegment todo
      where
        todo = error "LineSegment_x_LineSegment_LineSegment, not yet implemented"
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



--------------------------------------------------------------------------------

type instance Intersection (LineSegment endPoint point :+ extra)
                           (LineSegment endPoint point :+ extra) =
  Maybe (LineSegmentLineSegmentIntersection (LineSegment endPoint point :+ extra))
  -- FIXME: hmm, this type is kind of nonsense, since the intersecting segment may be from
  -- different segments

-- instance ( LineSegment endPoint point `HasIntersectionWith` LineSegment endPoint point
--          ) => (LineSegment endPoint point :+ extra) `HasIntersectionWith`
--               (LineSegment endPoint point :+ extra') where
--   (s :+ _) `intersects` (s' :+ _) = s `intersects` s'

instance ( LineSegment endPoint point `IsIntersectableWith` LineSegment endPoint point
         , Intersection (LineSegment endPoint point) (LineSegment endPoint point)
           ~ Maybe (LineSegmentLineSegmentIntersection (LineSegment endPoint point))
         ) => (LineSegment endPoint point :+ extra) `IsIntersectableWith`
              (LineSegment endPoint point :+ extra) where
  (s :+ _) `intersect` (s' :+ _) = fmap' (:+ undef') <$> s `intersect` s'
    where
      undef' = error "intersect semgnets: not possible"
