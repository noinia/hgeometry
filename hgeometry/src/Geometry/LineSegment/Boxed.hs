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
import Data.Foldable (minimumBy)
import Data.Ord (comparing)
import Geometry.Boundary
import Geometry.Interval.Boxed
import Geometry.Interval.Class
import Geometry.Interval.EndPoint
import Geometry.Line.Class
import Geometry.Line.Internal
import Geometry.LineSegment.Class
import Geometry.Point
import Geometry.Properties
import Geometry.Vector


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


type instance Dimension (LineSegmentF endPoint d point r) = d
type instance NumType   (LineSegmentF endPoint d point r) = r

--------------------------------------------------------------------------------


-- | Iso for turning a LineSegment into an interval.
_WrappedInterval :: Iso (LineSegmentF endPoint  d point r)
                        (LineSegmentF endPoint' d' point' r')
                        (Interval endPoint (point d r))
                        (Interval endPoint' (point' d' r'))
_WrappedInterval = iso (\(MkLineSegment i) -> i) MkLineSegment


instance (Functor endPoint, Functor (point d)) => Functor (LineSegmentF endPoint d point) where
  fmap f (LineSegment s t) = LineSegment (fmap (fmap f) s) (fmap (fmap f) t)
instance (Foldable endPoint, Foldable (point d)) => Foldable (LineSegmentF endPoint d point) where
  foldMap f (LineSegment s t) = foldMap (foldMap f) s <> foldMap (foldMap f) t
instance (Traversable endPoint, Traversable (point d))
         => Traversable (LineSegmentF endPoint d point) where
  traverse f (LineSegment s t) = LineSegment <$> traverse (traverse f) s
                                             <*> traverse (traverse f) t

instance EndPoint_ endPoint => HasStart (LineSegmentF endPoint d point r) (point d r) where
  start = _WrappedInterval.start.endPoint @endPoint

instance EndPoint_ endPoint => HasEnd   (LineSegmentF endPoint d point r) (point d r) where
  end   = _WrappedInterval.end.endPoint @endPoint


type instance NumType   (LineSegmentF endPoint d point r) = r
type instance Dimension (LineSegmentF endPoint d point r) = d

instance Traversable endPoint => HasPoints (LineSegmentF endPoint d point r)
                                           (LineSegmentF endPoint d point' r) point point' where
  allPoints f (LineSegment s t) =
    LineSegment <$> traverse f s <*> traverse f t


instance (Point_ point d r, EndPoint_ endPoint, Num r)
         => HasSupportingLine (LineSegmentF endPoint d point r) where
  supportingLine (LineSegment p q) = lineThrough (p^.endPoint) (q^.endPoint)

instance (EndPoint_ endPoint, Point_ point d r)
         => LineSegment_ (LineSegmentF endPoint) d point r where

  uncheckedLineSegment s t = LineSegment (mkEndPoint s) (mkEndPoint t)


instance (EndPoint_ endPoint, Point_ point d r, Fractional r)
         => OnSegment (LineSegmentF endPoint) d point r where

  (fromGenericPoint @point-> q) `onSegment` seg = case scalarMultiple (q .-. u) (v .-. u) of
                        Nothing -> False
                        Just q' -> q' `inInterval` toInterval seg /= Outside
    where
      u = seg^.start
      v = seg^.end

-- | get an interval from 0 to 1 corresponding to the segment, in
-- particular, keeps the endpoints as they were.
toInterval     :: (Functor endPoint, Num r)
               => (LineSegmentF endPoint) d point r -> Interval endPoint r
toInterval seg = let Interval s t = seg^._WrappedInterval
                 in Interval (0 <$ s) (1 <$ t)

instance (Fractional r, Ord r, Point_ point d r)
         => HasSquaredEuclideanDistance (ClosedLineSegment d point r) where
  -- pointClosestToWithDistance   :: (Num (NumType g), Point_ point (Dimension g) (NumType g))
  --                              => point (Dimension g) (NumType g) -> g
  --                              -> (point (Dimension g) (NumType g), NumType g)
  pointClosestToWithDistance q = sqDistanceToSegArg (fromGenericPoint @point q)

-- | Squared distance from the point to the Segment s, and the point on s
-- realizing it.
--
-- Note that if the segment is *open*, the closest point returned may
-- be one of the (open) end points, even though technically the end
-- point does not lie on the segment. (The true closest point then
-- lies arbitrarily close to the end point).
--
-- >>> :{
-- let ls = OpenLineSegment (Point2 0 0) (Point2 1 0)
--     p  = Point2 2 0
-- in  fst (sqDistanceToSegArg p ls) == Point2 1 0
-- :}
-- True
sqDistanceToSegArg                               :: forall point d r.
                                                    ( Ord r
                                                    , Fractional r
                                                    , Point_ point d r)
                                                 => point d r -> ClosedLineSegment d point r
                                                 -> (Point d r, r)
sqDistanceToSegArg q seg@(ClosedLineSegment s t) = minimumBy (comparing snd) pts
  where
    pts :: [(Point d r, r)]
    pts = (if fst m `onSegment` seg then (m :) else id) [f s, f t]
    f   :: point d r -> (Point d r, r)
    f a = (fromGenericPoint q,squaredEuclideanDist a q)
    m   = pointClosestToWithDistance q (supportingLine seg)
