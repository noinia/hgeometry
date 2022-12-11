{-# LANGUAGE UndecidableInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.LineSegment.Optimal
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Line segment data type and some basic functions on line segments
--
--------------------------------------------------------------------------------
module HGeometry.LineSegment.Optimal
  ( LineSegment(LineSegment, ClosedLineSegment, OpenLineSegment)
  , ClosedLineSegment
  , OpenLineSegment
  ) where


import Control.Lens
import Data.Functor.Apply
import Data.Kind (Type)
import HGeometry.Box.Boxable
import HGeometry.Interval.Class
import HGeometry.Interval.EndPoint
import HGeometry.Interval.Optimal
import HGeometry.Line.PointAndVector
import HGeometry.LineSegment.Class
import HGeometry.Point
import HGeometry.Properties
import HGeometry.Vector

--------------------------------------------------------------------------------

-- | Data type representing intervals
type LineSegment :: (Type -> Type) -> Type -> Type
newtype LineSegment endPoint point = MkLineSegment (Interval endPoint point)

-- | Default implementation of Closed LineSegments
type ClosedLineSegment point = LineSegment (EndPoint Closed) point

-- | Default implementation of Open LineSegments
type OpenLineSegment point   = LineSegment (EndPoint Open) point

-- | Construct a line Segment
pattern LineSegment     :: OptCVector_ 2 (endPoint point)
                        => endPoint point -> endPoint point -> LineSegment endPoint point
pattern LineSegment p q = MkLineSegment (Interval p q)
{-# COMPLETE LineSegment #-}

-- | Construct a closed interval
pattern ClosedLineSegment     :: OptCVector_ 2 point
                              =>  point -> point -> ClosedLineSegment point
pattern ClosedLineSegment s t = LineSegment (ClosedE s) (ClosedE t)
{-# COMPLETE ClosedLineSegment #-}

-- | Construct an open ended interval
pattern OpenLineSegment     :: OptCVector_ 2 point
                            =>  point -> point -> OpenLineSegment point
pattern OpenLineSegment s t = LineSegment (OpenE s) (OpenE t)
{-# COMPLETE OpenLineSegment #-}

type instance NumType   (LineSegment endPoint point) = NumType point
type instance Dimension (LineSegment endPoint point) = Dimension point


-- | Lens to get the underlying interval
_LineSegmentInterval :: Iso (LineSegment endPoint point) (LineSegment endPoint' point')
                            (Interval endPoint point)    (Interval    endPoint' point')
_LineSegmentInterval = iso (\(MkLineSegment i) -> i) MkLineSegment

instance ( IxValue (endPoint point) ~ point
         , OptCVector_ 2 (endPoint point)
         , EndPoint_ (endPoint point)
         ) => HasStart (LineSegment endPoint point) point where
  start = _LineSegmentInterval.start

instance ( IxValue (endPoint point) ~ point
         , OptCVector_ 2 (endPoint point)
         ) => HasStartPoint (LineSegment endPoint point) (endPoint point) where
  startPoint = _LineSegmentInterval.startPoint


instance ( IxValue (endPoint point) ~ point
         , OptCVector_ 2 (endPoint point)
         , EndPoint_ (endPoint point)
         ) => HasEnd (LineSegment endPoint point) point where
  end = _LineSegmentInterval.end

instance ( IxValue (endPoint point) ~ point
         , OptCVector_ 2 (endPoint point)
         ) => HasEndPoint (LineSegment endPoint point) (endPoint point) where
  endPoint = _LineSegmentInterval.endPoint

type instance EndPointOf (LineSegment endPoint point) = endPoint point

instance ( IxValue (endPoint point) ~ point
         , OptCVector_ 2 (endPoint point)
         , EndPoint_ (endPoint point)
         ) => IntervalLike_ (LineSegment endPoint point) point where
  mkInterval = LineSegment

instance ( IxValue (endPoint point) ~ point
         , OptCVector_ 2 (endPoint point)
         , EndPoint_ (endPoint point)
         , Point_ point (Dimension point) (NumType point)
         ) => LineSegment_ (LineSegment endPoint point) point where

instance ( OptCVector_ 2 (endPoint point)
         , OptCVector_ 2 (endPoint point')
         , Traversable1 endPoint
         , Dimension point ~ Dimension point'
         , Point_ point  (Dimension point) (NumType point)
         , Point_ point' (Dimension point) (NumType point')
         ) => HasPoints (LineSegment endPoint point) (LineSegment endPoint point')
                        point                        point' where
  allPoints f (LineSegment s t) = liftF2 LineSegment (traverse1 f s) (traverse1 f t)


instance ( OptCVector_ 2 (endPoint point)
         , Traversable1 endPoint
         , Point_ point  (Dimension point) (NumType point)
         ) => IsBoxable (LineSegment endPoint point)


instance ( Show (endPoint point)
         , OptCVector_ 2 (endPoint point)
         ) => Show (LineSegment endPoint point) where
  showsPrec k (LineSegment s t) = showParen (k > app_prec) $
                                    showString "LineSegment "
                                    . showsPrec (k+1) s
                                    . showChar ' '
                                    . showsPrec (k+1) t
    where
      app_prec = 10





testseg :: ClosedLineSegment (Point 2 Double)
testseg = ClosedLineSegment (Point2 5.0 6.0) (Point2 10.0 10.0)

test :: Point 2 Double
test = interpolate 0.5 testseg

instance ( d ~ Dimension point, r ~ NumType point
         , Point_ point d r
         , OptVector_ d r
         , OptMetric_ d r
         , EndPoint_ (endPoint point)
         , IxValue (endPoint point) ~ point
         , OptCVector_ 2 (endPoint point)
         , Num r
         ) => HasSupportingLine (LineSegment endPoint point) where
  supportingLine seg = let s = seg^.start.to pointFromPoint
                           t = seg^.end.to pointFromPoint
                       in Line s (t .-. s)

instance ( d ~ Dimension point, r ~ NumType point
         , Fractional r, Ord r
         , HasSquaredEuclideanDistance point
         , Point_ point d r
         , OptCVector_ 2 point
         , OptCVector_ 2 (EndPoint Closed point)
         , OptMetric_ d r
         ) => HasSquaredEuclideanDistance (ClosedLineSegment point) where
  pointClosestToWithDistance q seg@(ClosedLineSegment a b)
      | m `intersects` seg = z
      | otherwise          = minOn snd (pointClosestToWithDistance q a)
                                       (pointClosestToWithDistance q b)
    where
      z       :: (Point d r, r)
      z@(m,_) = pointClosestToWithDistance q (supportingLine seg)

      minOn       :: Ord b => (a -> b) -> a -> a -> a
      minOn f x y = if f x <= f y then x else y

-- type Intersection (Point d r) (ClosedLineSegment point)

instance OnSegment (LineSegment endPoint point) =>
         (Point d r) `HasIntersectionWith` (LineSegment endPoint point) where
  intersects = onSegment

-- instance OnSegment (LineSegment endPoint point)
