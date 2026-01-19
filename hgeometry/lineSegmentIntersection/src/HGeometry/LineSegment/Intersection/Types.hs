{-# LANGUAGE UndecidableInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.LineSegment.Intersection.Types
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Types for line segment intersections
--
--------------------------------------------------------------------------------
module HGeometry.LineSegment.Intersection.Types
  ( Intersections
  , intersectionPoints

  , Associated(Associated)
  , startPointOf, endPointOf, interiorTo
  , empty
  , mkAssociated
  , mkAroundStart, mkAroundEnd
  , associatedSegments

  , AroundEnd(..), AroundStart(..), AroundIntersection(..)
  , isInteriorIntersection


  , IntersectionPoint
  , intersectionPointOf

  , intersectionPoint, associatedSegs
  , mkIntersectionPoint


  , IntersectConstraints
  , OrdArounds

  , ordPoints


  , fromInteriors
  , mergeInteriorsWith
  ) where

import Control.Applicative((<|>))
import Control.DeepSeq
import Control.Lens
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Ord (comparing, Down(..))
import Data.Set qualified as Set
import GHC.Generics
import HGeometry.Intersection
import HGeometry.Line
import HGeometry.Properties
import HGeometry.Interval ()
import HGeometry.LineSegment
import HGeometry.Point
import HGeometry.Algorithms.DivideAndConquer (mergeSortedListsBy)
import Data.Coerce

----------------------------------------------------------------------------------


-- FIXME: What do we do when one segment lies *on* the other one. For
-- the short segment it should be an "around start", but then the
-- startpoints do not match.
--
-- for the long one it's an "on" segment, but they do not intersect


-- | A newtype helping us order segments CCW around their common start
-- point. I.e. this assumes that two segments have the same start
-- point.
newtype AroundStart a = AroundStart a
  deriving (Show,NFData,Functor,Generic)

instance Wrapped (AroundStart a) where
  type Unwrapped (AroundStart a) = a

instance (AroundStart a ~ t) => Rewrapped (AroundStart a) t

instance ( Point_ point 2 r, Eq r
         , HasEnd lineSegment point) => Eq (AroundStart lineSegment) where
  -- | equality on endpoint
  (AroundStart s) == (AroundStart s') = s^.end.asPoint == s'^.end.asPoint

instance ( LineSegment_ lineSegment point
         , Point_ point 2 r
         , Ord r, Num r
         ) => Ord (AroundStart lineSegment) where
  -- | ccw ordered around their suposed common startpoint
  (AroundStart s) `compare` (AroundStart s') = ccwCmpAround (s^.start) (s^.end)  (s'^.end)

----------------------------------------

-- | Assumes that two segments have the same end point (ordering is CCW around common endpoint)
-- (note that we specifically mean end point; not startpoint. See AroundStart)
newtype AroundEnd a = AroundEnd a deriving (Show,NFData,Functor,Generic)

instance Wrapped (AroundEnd a) where
  type Unwrapped (AroundEnd a) = a

instance (AroundEnd a ~ t) => Rewrapped (AroundEnd a) t

instance (Point_ point 2 r, Eq r, HasStart lineSegment point) => Eq (AroundEnd lineSegment) where
  -- | equality on endpoint
  (AroundEnd s) == (AroundEnd s') = s^.start.asPoint == s'^.start.asPoint

instance ( LineSegment_ lineSegment point
         , Point_ point 2 r
         , Ord r, Num r
         , Eq lineSegment
         ) => Ord (AroundEnd lineSegment) where
  -- | ccw ordered around their suposed common end point
  (AroundEnd s) `compare` (AroundEnd s') = ccwCmpAround (s^.end) (s^.start) (s'^.start)

--------------------------------------------------------------------------------

-- | This type represents a line segment seg, that contains some
-- globally known point "p" in its interior. The Ord instance of
-- 'AroundIntersection' orders segments of this type "around" p. In
-- particular, it orders the segments in CCW order by their starting
-- point (starting from the positive x-axis).
newtype AroundIntersection a = AroundIntersection a
  deriving (Eq,Show,NFData,Functor,Generic)

instance Wrapped (AroundIntersection a) where
    type Unwrapped (AroundIntersection a) = a

instance (AroundIntersection a ~ t) => Rewrapped (AroundIntersection a) t

-- instance ( LineSegment_ lineSegment point
--          , Point_ point 2 r
--          , Ord r, Fractional r
--          , Eq lineSegment
--          , IsIntersectableWith lineSegment lineSegment
--          , Intersection lineSegment lineSegment ~
--            Maybe (LineSegmentLineSegmentIntersection lineSegment')
--          , NumType lineSegment' ~ r
--          ) => Ord (AroundIntersection lineSegment) where
--   -- | ccw ordered around their common intersection point.
--   (AroundIntersection s) `compare` (AroundIntersection s') = case s `intersect` s' of
--     Nothing                                  ->
--       error "AroundIntersection: segments do not intersect!"
--     Just (LineSegment_x_LineSegment_Point p)       -> cmpAroundP p s s'
--     Just (LineSegment_x_LineSegment_LineSegment _) ->
--       error "BOOM!"
--       -- squaredLength s `compare` (squaredLength s')

--         -- if s and s' just happen to be the same length but
--         -- intersect in different behaviour from using (==).
--         -- but that situation doese not satisfy the precondition
--         -- of aroundIntersection anyway.
--     where
--       squaredLength ss = squaredEuclideanDist (ss^.start) (ss^.end)
-- -- FIXME: should this instance really exist?


-- | We compare the segments by their startPoints, ordered CCCW around
-- p, starting from the positive x-axis.
cmpAroundP        :: ( LineSegment_ lineSegment point
                     , Point_ point 2 r
                     , Point_ point' 2 r
                     , Ord r, Num r
                     )
                  => point' -> lineSegment -> lineSegment -> Ordering
cmpAroundP p s s' = ccwCmpAround (p^.asPoint) (s^.start.asPoint)  (s'^.start.asPoint)





-- seg1 = ClosedLineSegment (ext $ Point2 0 0) (ext $ Point2 0 10)
-- seg2 = ClosedLineSegment (ext $ Point2 0 0) (ext $ Point2 0 10)

--------------------------------------------------------------------------------



-- | The line segments that contain a given point p may either have p
-- as the endpoint or have p in their interior.
--
-- if somehow the segment is degenerate, and p is both the start and
-- end it is reported only as the start point.
data Associated lineSegment =
  Associated { _startPointOf :: Set.Set (AroundStart lineSegment)
             -- ^ segments for which the intersection point is the
             -- start point (i.e. s^.start == p). These segments are
             -- stored in CCW order around their common starting point.
             , _endPointOf   :: Set.Set (AroundEnd lineSegment)
             -- ^ segments for which the intersection point is the end
             -- point (i.e. s^.end == p)
             , _interiorTo   :: Set.Set (AroundIntersection lineSegment)
             } deriving stock (Show, Generic)


deriving stock instance ( Eq (AroundStart lineSegment)
                        , Eq (AroundIntersection lineSegment)
                        , Eq (AroundEnd lineSegment)
                        ) => Eq (Associated lineSegment)

-- deriving stock instance ( Read lineSegment
--                         , OrdArounds lineSegment
--                         ) => Read (Associated lineSegment)

-- | Constructs an empty associated
empty :: Associated lineSegment
empty = Associated Set.empty Set.empty Set.empty


-- | Shorthand for the required Ord instances
type OrdArounds lineSegment = ( Ord (AroundStart lineSegment)
                              -- , Ord (AroundIntersection lineSegment)
                              , Ord (AroundEnd lineSegment)
                              )

-- | Lens to access the segments for which this is a startPoint
startPointOf :: Lens' (Associated lineSegment) (Set.Set (AroundStart lineSegment))
startPointOf f (Associated ss es is) = fmap (\ss' -> Associated ss' es is) (f ss)
{-# INLINE startPointOf #-}

-- | Lens to access the segments for which this is an endPoint
endPointOf :: Lens' (Associated lineSegment) (Set.Set (AroundEnd lineSegment))
endPointOf f (Associated ss es is) = fmap (\es' -> Associated ss es' is) (f es)
{-# INLINE endPointOf #-}

-- | Lens to access the segments for which this point lies in the interior of the segment
interiorTo :: Lens' (Associated lineSegment) (Set.Set (AroundIntersection lineSegment))
interiorTo f (Associated ss es is) = fmap (\is' -> Associated ss es is') (f is)
{-# INLINE interiorTo #-}


-- | Fold over the segments associated with the intersection.
associatedSegments     :: Fold (Associated lineSegment) lineSegment
associatedSegments f a =  (startPointOf . folded . _Wrapped) f a *>
                          (endPointOf   . folded . _Wrapped) f a *>
                          (interiorTo   . folded . _Wrapped) f a
  -- combine the folds

-- instance Functor (Associated lineSegment) where
--   fmap f (Associated ss es is) = Associated (Set.mapMonotonic (g f) ss)
--                                             (Set.mapMonotonic (g f) es)
--                                             (Set.mapMonotonic (g f) is)
--     where
--       g   :: forall f c e b. Functor f => (e -> b) -> f (c :+ e) -> f (c :+ b)
--       g f' = fmap (&extra %~ f')


-- | Reports whether this associated has any interior intersections
--
-- \(O(1)\)
isInteriorIntersection :: Associated lineSegment -> Bool
isInteriorIntersection = not . null . _interiorTo


-- | Constructs an around start
mkAroundStart   :: lineSegment -> Associated lineSegment
mkAroundStart s = empty&startPointOf .~  Set.singleton (AroundStart s)

-- | Constructs an ArroundEnd
mkAroundEnd   :: lineSegment -> Associated lineSegment
mkAroundEnd s = empty&endPointOf   .~  Set.singleton (AroundEnd s)


-- | test if the given segment has p as its endpoint, an construct the
-- appropriate associated representing that.
--
-- pre: p intersects the segment
mkAssociated      :: ( LineSegment_ lineSegment point
                     , Point_ point 2 r
                     , Point_ point' 2 r
                     , Eq r
                     )
                  => point' -> lineSegment -> Associated lineSegment
mkAssociated p s
  | p^.asPoint == s^.start.asPoint = empty&startPointOf .~  Set.singleton (AroundStart s)
  | p^.asPoint == s^.end.asPoint   = empty&endPointOf   .~  Set.singleton (AroundEnd s)
  | otherwise                      = empty&interiorTo   .~  Set.singleton (AroundIntersection s)





---- | test if the given segment has p as its endpoint, an construct the
---- appropriate associated representing that.
----
---- If p is not one of the endpoints we concstruct an empty Associated!
----
--mkAssociated'     :: ( LineSegment_ lineSegment point
--                     , Point_ point 2 r
--                     , Eq r
--                     , OrdArounds lineSegment
--                     )
--                  => point -> lineSegment -> Associated lineSegment
--mkAssociated' p s = (mkAssociated p s)&interiorTo .~ mempty


instance ( OrdArounds lineSegment
         , LineSegment_ lineSegment point, Point_ point 2 r, Ord r, Fractional r
         , HasSupportingLine lineSegment
         ) => Semigroup (Associated lineSegment) where
  (Associated ss es is) <> (Associated ss' es' is') =
    Associated starts ends (mergeInteriors is is' mp)
    where
      starts = ss <> ss'
      ends   = es <> es'
      -- try to find the intersection point p that this Associated represents
      mp     = firstOf (folded._Wrapped.start.asPoint) starts
               <|> firstOf (folded._Wrapped.end.asPoint) ends

-- | Merge the interiors of the Associated. The Maybe (Point 2 r) is
-- the intersection point in which all the segments intersect in their
-- interior (if we already know it; e.g. because it was the start or
-- endpoint of one of the other segments that also intersect here).
--
-- If this value is Nothing, then it must mean that all given segments
-- actually are interior intersections.
--
-- pre: if Nothing, then not all segments are colinear. This should be the
-- case (at least assuming we don't have )
mergeInteriors        :: forall lineSegment endPoint r.
                         ( LineSegment_ lineSegment endPoint
                         , Point_ endPoint 2 r, Ord r, Fractional r
                         , HasSupportingLine lineSegment
                         )
                      => Set.Set (AroundIntersection lineSegment)
                      -> Set.Set (AroundIntersection lineSegment)
                      -> Maybe (Point 2 r)
                      -> Set.Set (AroundIntersection lineSegment)
mergeInteriors is is' = \case
    Just p  -> mergeInteriorsWith p is is'
    Nothing -> case Set.minView is of
      Nothing    -> is' -- nothing to mrege anyway
      Just (s,rest) -> case Set.minView is' of
        Nothing -> is -- nothing to merge anyway
        Just _  -> mergeInteriorsWith (findInteriorIntersection s rest) is is'
                   -- in this case, we claim there must be some segment that is not
                   -- colinear with s.
  where
    findInteriorIntersection (AroundIntersection s@(LineSegment_ a b)) rest =
      case filter nonColinear $
             coerce @_ @[lineSegment] (Set.toAscList rest <> Set.toAscList is') of
        (s':_) -> case supportingLine s `intersect` supportingLine s' of
          Just (Line_x_Line_Point p) -> p
          _                          -> error "mergeInteriors. absurd. non-colinear intersect in point"
        _      -> error "mergeInteriors. no non-colinear segments !?"
      where
        nonColinear s' = ccw a b (s'^.start) /= CoLinear || ccw a b (s'^.end) /= CoLinear

-- | Merge the two 'AroundIntersection' sets; given that they all have
-- point p in their interior.
mergeInteriorsWith ::       forall lineSegment endPoint r.
                            ( LineSegment_ lineSegment endPoint
                            , Point_ endPoint 2 r, Ord r, Num r
                            ) => Point 2 r
                            -> Set.Set (AroundIntersection lineSegment)
                            -> Set.Set (AroundIntersection lineSegment)
                            -> Set.Set (AroundIntersection lineSegment)
mergeInteriorsWith p is is' = Set.fromDistinctAscList . coerce
                            $ mergeSortedListsBy (cmpInteriors p)
                                                 (coerce @_ @[lineSegment] $ Set.toAscList is)
                                                 (coerce $ Set.toAscList is')

instance ( OrdArounds lineSegment
         , LineSegment_ lineSegment point, Point_ point 2 r, Ord r, Fractional r
         , HasSupportingLine lineSegment
         ) => Monoid (Associated lineSegment) where
  mempty = empty

instance (NFData lineSegment) => NFData (Associated lineSegment)

-- | For each intersection point the segments intersecting there.
type Intersections r lineSegment = Map.Map (Point 2 r) (Associated lineSegment)

-- | Get the set of all intersection points
intersectionPoints :: Intersections r lineSegment -> Set.Set (Point 2 r)
intersectionPoints = Map.keysSet

-- | An intersection point together with all segments intersecting at
-- this point.
data IntersectionPoint point lineSegment =
  IntersectionPoint { _intersectionPoint :: !point
                    , _associatedSegs    :: !(Associated lineSegment)
                    } deriving stock (Show,Generic)

-- | Lens to access the intersectionp oint
intersectionPoint :: Lens (IntersectionPoint point lineSegment)
                          (IntersectionPoint point' lineSegment)
                          point point'
intersectionPoint f (IntersectionPoint p ss) = fmap (\p' -> IntersectionPoint p' ss) (f p)
{-# INLINE intersectionPoint #-}

-- | Lens to access the associated segments
associatedSegs :: Lens (IntersectionPoint point lineSegment)
                       (IntersectionPoint point lineSegment')
                       (Associated lineSegment) (Associated lineSegment')
associatedSegs f (IntersectionPoint p ss) = fmap (\ss' -> IntersectionPoint p ss') (f ss)
{-# INLINE associatedSegs #-}


deriving stock instance ( Eq (AroundStart lineSegment)
                        , Eq (AroundIntersection lineSegment)
                        , Eq (AroundEnd lineSegment)
                        , Eq point
                        ) => Eq (IntersectionPoint point lineSegment)

-- deriving stock instance ( Read lineSegment, Read point
--                         , OrdArounds lineSegment
--                         ) => Read (IntersectionPoint point lineSegment)


instance (NFData point, NFData lineSegment) => NFData (IntersectionPoint point lineSegment)




-- | Given a point p, and a bunch of segments that suposedly intersect
-- at p, correctly categorize them.
mkIntersectionPoint         :: ( LineSegment_ lineSegment endPoint
                               , Point_ endPoint 2 r
                               , Point_ point 2 r, Ord r, Num r
                               , OrdArounds lineSegment
                               )
                            => point
                            -> [lineSegment] -- ^ uncategorized
                            -> [lineSegment] -- ^ segments we know contain p,
                            -> IntersectionPoint point lineSegment
mkIntersectionPoint p as cs = IntersectionPoint p $ Associated starts ends interiors
  where
    p' = p^.asPoint
    (starts',ends') = List.partition (\seg -> seg^.start.asPoint == p') as
    starts    = foldMap (Set.singleton . AroundStart) starts'
    ends      = foldMap (Set.singleton . AroundEnd)   ends'
    interiors = fromInteriors p' cs


-- | Helper to produce the "AroundIntersection" part of the associate segments
fromInteriors      :: ( LineSegment_ lineSegment endPoint
                      , Point_ endPoint 2 r, Ord r, Num r
                      ) => Point 2 r -> [lineSegment] -> Set.Set (AroundIntersection lineSegment)
fromInteriors p cs =
  Set.fromDistinctAscList . map AroundIntersection . List.sortBy (cmpInteriors p) $ cs

-- | we first compare by the CCW orrientation of the startin point; on
-- equal starting angles, we prefer segments closer to p. If those are
-- equal as well, order by distance to the endpoint. (If those are
-- equal, the segments would really have the same endpoints; so
-- consider them equal
cmpInteriors        :: ( LineSegment_ lineSegment endPoint
                       , Point_ endPoint 2 r, Ord r, Num r
                       )
                    => Point 2 r -> lineSegment -> lineSegment -> Ordering
cmpInteriors p s s' = cmpAroundP p s s' <> cmpDist (s^.start.asPoint) (s'^.start.asPoint)
                                   <> cmpDist (s^.end.asPoint)   (s'^.end.asPoint)
  where
    -- compare by increasing distance to p.
    cmpDist = comparing (squaredEuclideanDist p)

-- | An ordering that is decreasing on y, increasing on x
ordPoints     :: (Point_ point 2 r, Ord r) => point -> point -> Ordering
ordPoints a b = let f p = (Down $ p^.yCoord, p^.xCoord) in comparing f a b

-- | Given two segments, compute an IntersectionPoint representing their intersection (if
-- such an intersection exists).
intersectionPointOf      :: ( LineSegment_ lineSegment point
                            , LineSegment_ seg point
                            , Point_ point 2 r
                            , Ord r, Fractional r
                            , IntersectConstraints seg lineSegment
                            )
                         => lineSegment -> lineSegment
                         -> Maybe (IntersectionPoint (Point 2 r) lineSegment)
intersectionPointOf s s' = s `intersect` s' <&> \case
     LineSegment_x_LineSegment_Point p         -> intersectionPoint' p
     LineSegment_x_LineSegment_LineSegment seg -> intersectionPoint' (topEndPoint seg)
  where
    intersectionPoint' p = IntersectionPoint p associated
      where
        associated = case categorize p s of
          Start    -> mkAssociated p s' & startPointOf %~ Set.insert (AroundStart s)
          End      -> mkAssociated p s' & endPointOf   %~ Set.insert (AroundEnd s)
          Interior -> case categorize p s' of
             Interior -> empty             & interiorTo .~ fromInteriors p [s,s']
             _        -> mkAssociated p s' & interiorTo .~ Set.singleton (AroundIntersection s)
    topEndPoint seg = List.minimumBy ordPoints [seg^.start.asPoint, seg^.end.asPoint]

data IntersectionType = Start | End | Interior deriving (Show,Eq)

-- | Try to find the apprpriate intersection type
categorize    :: (Eq r, LineSegment_ lineSegment point, Point_ point 2 r)
              => Point 2 r -> lineSegment -> IntersectionType
categorize p s
  | p == s^.start.asPoint = Start
  | p == s^.end.asPoint   = End
  | otherwise             = Interior


-- | Shorthand for the more-or-less standard constraints that we need on LineSegments
type IntersectConstraints seg lineSegment =
  ( OrdArounds lineSegment
  , IsIntersectableWith lineSegment lineSegment
  , Intersection lineSegment lineSegment ~ Maybe (LineSegmentLineSegmentIntersection seg)
  , NumType seg ~ NumType lineSegment
  , Dimension seg ~ Dimension lineSegment
  )
