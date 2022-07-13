{-# LANGUAGE UndecidableInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Geometry.LineSegment.Boxed
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- A data type for representing line segments
--
--------------------------------------------------------------------------------
module Geometry.LineSegment.Boxed
  ( LineSegment, pattern LineSegment
  , ClosedLineSegment, pattern ClosedLineSegment
  , OpenLineSegment, pattern OpenLineSegment


  , LineSegmentF
  ) where

import Control.Lens
import Geometry.Interval.Boxed
import Geometry.Interval.Class
import Geometry.Interval.EndPoint
import Geometry.LineSegment.Class
import Geometry.Point.Class
import Geometry.Properties

--------------------------------------------------------------------------------

-- | A line segment type supporting endpoints that may be open or closed.
type LineSegment       = LineSegmentF EndPoint

-- | A line segment type whose endpoints are both closed.
type ClosedLineSegment = LineSegmentF Closed


-- | A line segment type whose endpoints are both open.
type OpenLineSegment   = LineSegmentF Open

--------------------------------------------------------------------------------

-- | The generic Linesegment implementation.
newtype LineSegmentF endPoint d point r =
  MkLineSegment (Interval endPoint (point d r))

-- | Pattern for constructing line segments
pattern LineSegment     :: endPoint (point d r)
                        -> endPoint (point d r)
                        -> LineSegmentF endPoint d point r
pattern LineSegment s t = MkLineSegment (Interval s t)
{-# COMPLETE LineSegment #-}

-- | Pattern for specifically creating Closed line segments
pattern ClosedLineSegment     :: point d r -> point d r -> ClosedLineSegment d point r
pattern ClosedLineSegment s t = LineSegment (Closed s) (Closed t)
{-# COMPLETE ClosedLineSegment #-}

-- | Pattern for specifically creating Open line segments
pattern OpenLineSegment     :: point d r -> point d r -> OpenLineSegment d point r
pattern OpenLineSegment s t = LineSegment (Open s) (Open t)
{-# COMPLETE OpenLineSegment #-}

--------------------------------------------------------------------------------


-- | Iso for turning a LineSegment into an interval.
_WrappedInterval :: Control.Lens.Iso (LineSegmentF endPoint  d point r)
                        (LineSegmentF endPoint' d' point' r')
                        (Interval endPoint (point d r))
                        (Interval endPoint' (point' d' r'))
_WrappedInterval = Control.Lens.iso (\(MkLineSegment i) -> i) MkLineSegment


instance (Functor endPoint, Functor (point d)) => Functor (LineSegmentF endPoint d point) where
  fmap f (LineSegment s t) = LineSegment (fmap (fmap f) s) (fmap (fmap f) t)
instance (Foldable endPoint, Foldable (point d)) => Foldable (LineSegmentF endPoint d point) where
  foldMap f (LineSegment s t) = foldMap (foldMap f) s <> foldMap (foldMap f) t
instance (Control.Lens.Traversable endPoint, Control.Lens.Traversable (point d))
         => Control.Lens.Traversable (LineSegmentF endPoint d point) where
  traverse f (LineSegment s t) = LineSegment <$> Control.Lens.traverse (Control.Lens.traverse f) s
                                             <*> Control.Lens.traverse (Control.Lens.traverse f) t

instance EndPoint_ endPoint => HasStart (LineSegmentF endPoint d point r) (point d r) where
  start = _WrappedInterval.start.endPoint @endPoint

instance EndPoint_ endPoint => HasEnd   (LineSegmentF endPoint d point r) (point d r) where
  end   = _WrappedInterval.end.endPoint @endPoint


type instance NumType   (LineSegmentF endPoint d point r) = r
type instance Dimension (LineSegmentF endPoint d point r) = d

instance Control.Lens.Traversable endPoint => HasPoints (LineSegmentF endPoint d point r)
                                           (LineSegmentF endPoint d point' r) point point' where
  allPoints f (LineSegment s t) = LineSegment <$> Control.Lens.traverse f s <*> Control.Lens.traverse f t

instance (EndPoint_ endPoint, Point_ point d r)
         => LineSegment_ (LineSegmentF endPoint) d point r where
  uncheckedLineSegment s t = LineSegment (mkEndPoint s) (mkEndPoint t)
