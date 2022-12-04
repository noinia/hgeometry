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
import HGeometry.Interval.EndPoint
import HGeometry.Interval.Optimal
import HGeometry.Interval.Class
import HGeometry.LineSegment.Class
import HGeometry.Properties
import HGeometry.Vector
import HGeometry.Point

--------------------------------------------------------------------------------

-- | Data type representing intervals
newtype LineSegment endPoint point = MkLineSegment (Interval endPoint point)

type ClosedLineSegment point = LineSegment (EndPoint Closed) point
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
