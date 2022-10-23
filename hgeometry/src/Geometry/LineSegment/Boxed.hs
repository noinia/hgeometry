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

  , flipSegment
  ) where

import Control.Lens
import Data.Foldable (minimumBy)
import Data.Ord (comparing)
import Data.Vinyl
import Data.Vinyl.CoRec
import Geometry.Boundary
import Geometry.Interval.Boxed
import Geometry.Interval.Class
import Geometry.Interval.EndPoint
import Geometry.Line.Class
import Geometry.Line.Internal
import Geometry.LineSegment.Class
import Geometry.Point
import Geometry.Properties
import Geometry.SubLine
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

--------------------------------------------------------------------------------


instance {-# OVERLAPPING #-} (EndPoint_ endPoint, Point_ point 2 r)
         => OnSegment (LineSegmentF endPoint) 2 point r where
  onSegment = onSegment2

instance {-# OVERLAPPING #-} (EndPoint_ endPoint, Point_ point d r, Fractional r)
         => OnSegment (LineSegmentF endPoint) d point r where
  onSegment = onSegmentFractional


onSegment2                         :: forall endPoint point point' r.
                                      ( Ord r, Num r, Point_ point' 2 r
                                      , EndPoint_ endPoint, Point_ point 2 r
                                      )
                                   => point' 2 r
                                   -> LineSegmentF endPoint 2 point r
                                   -> Bool
onSegment2 (fromGenericPoint @point -> q) seg@(LineSegment u v) =
    case ccw q (seg^.start) (seg^.end) of
      CoLinear -> let su = q `onSide` lu
                      sv = q `onSide` lv
                  in su /= sv
                     && ((su == OnLine) `implies` isClosed u)
                     && ((sv == OnLine) `implies` isClosed v)
      _        -> False
    where
      (Line _ w) = perpendicularTo $ supportingLine seg
      lu = Line (u^.endPoint.to fromGenericPoint) w
      lv = Line (v^.endPoint.to fromGenericPoint) w

      a `implies` b = b || not a
      isClosed p = endPointType p == ClosedEndPoint


onSegmentFractional :: forall endPoint point d point' r.
                       ( Ord r, Fractional r, Point_ point' d r
                       , EndPoint_ endPoint, Point_ point d r
                       )
                    => point' d r
                    -> LineSegmentF endPoint d point r
                    -> Bool
onSegmentFractional (fromGenericPoint @point-> q) seg = case scalarMultiple (q .-. u) (v .-. u) of
    Nothing -> False
    Just q' -> q' `inInterval` toInterval seg /= Outside
  where
    u = seg^.start
    v = seg^.end

--------------------------------------------------------------------------------

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
    pts = (if fst m `onSegmentFractional` seg then (m :) else id) [f s, f t]
    f   :: point d r -> (Point d r, r)
    f a = (fromGenericPoint q,squaredEuclideanDist a q)
    m   = pointClosestToWithDistance q (supportingLine seg)



--------------------------------------------------------------------------------

-- | flips the start and end point of the segment
flipSegment                   :: LineSegmentF endPoint d point r -> LineSegmentF endPoint d point r
flipSegment (LineSegment p q) = LineSegment q p





--------------------------------------------------------------------------------

type instance IntersectionOf (Point d r) (LineSegmentF endpoint d point r) =
  [ NoIntersection
  , Point d r
  ]

-- type instance IntersectionOf (LineSegment 2 p r) (LineSegment 2 p r) = [ NoIntersection
--                                                                        , Point 2 r
--                                                                        , LineSegment 2 p r
--                                                                        ]

type instance IntersectionOf (LineSegmentF endPoint 2 point r)
                             (LineSegmentF endPoint 2 point r) =
  [ NoIntersection, Point 2 r, LineSegment 2 point r]

type instance IntersectionOf (LineSegmentF endPoint 2 point r) (Line 2 r) =
  [ NoIntersection
  , Point 2 r
  , LineSegmentF endPoint 2 point r
  ]

instance {-# OVERLAPPING #-} (Ord r, Num r, EndPoint_ endPoint, Point_ point 2 r)
         => Point 2 r `HasIntersectionWith` (LineSegmentF endPoint 2 point r) where
  intersects = onSegment2

instance {-# OVERLAPPING #-} (Ord r, Num r, EndPoint_ endPoint, Point_ point 2 r)
         => Point 2 r `IsIntersectableWith` (LineSegmentF endPoint 2 point r) where
  nonEmptyIntersection = defaultNonEmptyIntersection
  p `intersect` seg | p `intersects` seg = coRec p
                    | otherwise          = coRec NoIntersection


instance {-# OVERLAPPABLE #-} (Ord r, Fractional r, EndPoint_ endPoint, Point_ point d r)
         => Point d r `HasIntersectionWith` (LineSegmentF endPoint d point r) where
  intersects = onSegmentFractional

instance {-# OVERLAPPABLE #-} (Ord r, Fractional r, EndPoint_ endPoint, Point_ point d r)
         => Point d r `IsIntersectableWith` (LineSegmentF endPoint d point r) where
  nonEmptyIntersection = defaultNonEmptyIntersection
  p `intersect` seg | p `intersects` seg = coRec p
                    | otherwise          = coRec NoIntersection


--------------------------------------------------------------------------------

withRank = undefined
-- -- | Orders the endpoints of the segments in the given direction.
-- withRank                                       :: forall p q r. (Ord r, Num r)
--                                                => Vector 2 r
--                                                -> LineSegmentF endPoint 2 point r
--                                                -> LineSegmentF endPoint 2 point' r
--                                                -> (Interval point Int, Interval point' Int)
-- withRank v (LineSegment p q) (LineSegment a b) = (i1,i2)
--   where
--     -- let rank p = 3, rank q = 6
--     i1 = Interval (p&unEndPoint.core .~ 3) (q&unEndPoint.core .~ 6)

--     i2 = Interval (a&unEndPoint.core .~ assign' 1 a') (a&unEndPoint.core .~ assign' 2 b')

--     -- make sure the intervals are in the same order, otherwise flip them.
--     (a',b') = case cmp a b of
--                 LT -> (a,b)
--                 EQ -> (a,b)
--                 GT -> (b,a)

--     assign' x c = case cmp c p of
--                     LT -> x
--                     EQ -> 3
--                     GT -> case cmp c q of
--                             LT -> 4 + x
--                             EQ -> 6
--                             GT -> 7 + x

--     cmp     :: EndPoint (Point 2 r :+ a) -> EndPoint (Point 2 r :+ b) -> Ordering
--     cmp c d = cmpInDirection v (c^.unEndPoint.core) (d^.unEndPoint.core)

instance (Ord r, Num r
         , LineSegment_ (LineSegmentF endPoint) 2 point r
         , Point_ point 2 r, EndPoint_ endPoint
         ) =>
         LineSegmentF endPoint 2 point r `HasIntersectionWith`
         LineSegmentF endPoint 2 point r where
  s1 `intersects` s2
    | l1 `isParallelTo2` l2 = parallelCase
    | otherwise             = s1 `intersects` l2  && s2 `intersects` l1
    where
      l1@(Line _ v) = supportingLine s1
      l2 = supportingLine s2

      parallelCase = (s1^.start :: point 2 r) `onLine2` l2 && i1 `intersects` i2
      (i1,i2) = withRank v s1 s2

    -- correctness argument:
    -- if the segments share a supportingLine (l1 and l2 parallel, and point of l1 on l2)
    -- the segments intersect iff their intervals along the line intersect.

    -- if the supporting lines intersect in a point, say x the
    -- segments intersect iff s1 intersects the supporting line and
    -- vice versa:
    ---
    -- => direction: is trivial
    -- <= direction: s1 intersects l2 means x
    -- lies on s1. Symmetrically s2 intersects l1 means x lies on
    -- s2. Hence, x lies on both s1 and s2, and thus the segments
    -- intersect.






-- instance (Ord r, Fractional r) =>
--          LineSegmentF endPoint 2 point r `IsIntersectableWith`
--          LineSegmentF endPoint 2 point r where
--   nonEmptyIntersection = defaultNonEmptyIntersection

--   a `intersect` b = match ((a^._SubLine) `intersect` (b^._SubLine)) $
--          H coRec
--       :& H coRec
--       :& H (coRec . subLineToSegment)
--       :& RNil

instance (Ord r, Num r
         , LineSegment_ (LineSegmentF endPoint) 2 point r
         , Point_ point 2 r, EndPoint_ endPoint
         ) =>
         LineSegmentF endPoint 2 point r `HasIntersectionWith` Line 2 r where
  seg@(LineSegment p q) `intersects` l = case onSide (p^.endPoint) l of
    OnLine -> isClosed p || case onSide (p^.endPoint) l of
                              OnLine -> isClosed q
                                        || (p^.endPoint.asVector) /= (q^.endPoint.asVector)
                              _      -> False
    sp     -> case onSide (seg^.end :: point 2 r) l of
                OnLine -> isClosed q
                sq     -> sp /= sq
    where
      isClosed a = endPointType a == ClosedEndPoint


instance ( Ord r, Fractional r
         , LineSegment_ (LineSegmentF endPoint) 2 point r
         , Point_ point 2 r, EndPoint_ endPoint
         ) =>
         LineSegmentF endPoint 2 point r `IsIntersectableWith` Line 2 r where
  nonEmptyIntersection = defaultNonEmptyIntersection

  seg `intersect` l = match (supportingLine seg `intersect` l) $
         H  coRec
      :& H (\p -> if p `onSegment2` seg then coRec p else coRec NoIntersection)
      :& H (const (coRec seg))
      :& RNil
