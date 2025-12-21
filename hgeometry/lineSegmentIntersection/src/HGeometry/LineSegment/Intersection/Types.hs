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
  , empty, mkAssociated, mkAroundStart, mkAroundEnd
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
  ) where

import           Control.DeepSeq
import           Control.Lens
import qualified Data.List as List
import qualified Data.Map as Map
import           Data.Ord (comparing, Down(..))
import qualified Data.Set as Set
import           GHC.Generics
import           HGeometry.Intersection
import           HGeometry.Properties
import           HGeometry.Interval
import           HGeometry.LineSegment
import           HGeometry.Point

----------------------------------------------------------------------------------


-- FIXME: What do we do when one segment lies *on* the other one. For
-- the short segment it should be an "around start", but then the
-- startpoints do not match.
--
-- for the long one it's an "on" segment, but they do not intersect


-- | Assumes that two segments have the same start point
newtype AroundStart a = AroundStart a
  deriving (Show,Read,NFData,Functor,Generic)

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

-- | Assumes that two segments have the same end point
newtype AroundEnd a = AroundEnd a deriving (Show,Read,NFData,Functor,Generic)

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

-- | Assumes that two segments intersect in a single point.
newtype AroundIntersection a = AroundIntersection a
  deriving (Eq,Show,Read,NFData,Functor,Generic)

instance Wrapped (AroundIntersection a) where
    type Unwrapped (AroundIntersection a) = a

instance (AroundIntersection a ~ t) => Rewrapped (AroundIntersection a) t

instance ( LineSegment_ lineSegment point
         , Point_ point 2 r
         , Ord r, Fractional r
         , Eq lineSegment
         , IsIntersectableWith lineSegment lineSegment
         , Intersection lineSegment lineSegment ~
           Maybe (LineSegmentLineSegmentIntersection lineSegment')
         , NumType lineSegment' ~ r
         ) => Ord (AroundIntersection lineSegment) where
  -- | ccw ordered around their common intersection point.
  (AroundIntersection s) `compare` (AroundIntersection s') = case s `intersect` s' of
    Nothing                                  ->
      error "AroundIntersection: segments do not intersect!"
    Just (LineSegment_x_LineSegment_Point p)       -> cmpAroundP p s s'
    Just (LineSegment_x_LineSegment_LineSegment _) -> squaredLength s `compare` (squaredLength s')
        -- if s and s' just happen to be the same length but
        -- intersect in different behaviour from using (==).
        -- but that situation doese not satisfy the precondition
        -- of aroundIntersection anyway.
    where
      squaredLength ss = squaredEuclideanDist (ss^.start) (ss^.end)

-- | compare around p
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
             -- start point (i.e. s^.start == p)
             , _endPointOf   :: Set.Set (AroundEnd lineSegment)
             -- ^ segments for which the intersection point is the end
             -- point (i.e. s^.end == p)
             , _interiorTo   :: Set.Set (AroundIntersection lineSegment)
             } deriving stock (Show, Generic)

deriving stock instance ( Eq (AroundStart lineSegment)
                        , Eq (AroundIntersection lineSegment)
                        , Eq (AroundEnd lineSegment)
                        ) => Eq (Associated lineSegment)

deriving stock instance ( Read lineSegment
                        , OrdArounds lineSegment
                        ) => Read (Associated lineSegment)

-- | Constructs an empty associated
empty :: Associated lineSegment
empty = Associated Set.empty Set.empty Set.empty


-- | Shorthand for the required Ord instances
type OrdArounds lineSegment = ( Ord (AroundStart lineSegment)
                              , Ord (AroundIntersection lineSegment)
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


instance OrdArounds lineSegment => Semigroup (Associated lineSegment) where
  (Associated ss es is) <> (Associated ss' es' is') =
    Associated (ss <> ss') (es <> es') (is <> is')

instance OrdArounds lineSegment => Monoid (Associated lineSegment) where
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

deriving stock instance ( Read lineSegment, Read point
                        , OrdArounds lineSegment
                        ) => Read (IntersectionPoint point lineSegment)


instance (NFData point, NFData lineSegment) => NFData (IntersectionPoint point lineSegment)


-- sameOrder           :: (Ord r, Num r, Eq p) => Point 2 r
--                     -> [LineSegment 2 p r] -> [LineSegment 2 p r] -> Bool
-- sameOrder c ss ss' = f ss == f ss'
--   where
--     f = map (^.extra) . sortAround' (ext c) . map (\s -> s^.end.core :+ s)




-- | Given a point p, and a bunch of segments that suposedly intersect
-- at p, correctly categorize them.
mkIntersectionPoint         :: ( LineSegment_ lineSegment endPoint
                               , Point_ endPoint 2 r
                               , Point_ point 2 r, Eq r
                               , OrdArounds lineSegment
                               )
                            => point
                            -> [lineSegment] -- ^ uncategorized
                            -> [lineSegment] -- ^ segments we know contain p,
                            -> IntersectionPoint point lineSegment
mkIntersectionPoint p as cs = IntersectionPoint p $ foldMap (mkAssociated p) $ as <> cs

  -- IntersectionPoint p
  --                           $ Associated mempty mempty (Set.fromAscList cs')
  --                           <> foldMap (mkAssociated p) as
  -- where
  --   cs' = map AroundIntersection . List.sortBy (cmpAroundP p) $ cs
  -- -- TODO: In the bentley ottman algo we already know the sorted order of the segments
  -- -- so we can likely save the additional sort


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
    intersectionPoint' p = IntersectionPoint p (mkAssociated p s <> mkAssociated p s')
    topEndPoint seg = List.minimumBy ordPoints [seg^.start.asPoint, seg^.end.asPoint]

-- | Shorthand for the more-or-less standard constraints that we need on LineSegments
type IntersectConstraints seg lineSegment =
  ( OrdArounds lineSegment
  , IsIntersectableWith lineSegment lineSegment
  , Intersection lineSegment lineSegment ~ Maybe (LineSegmentLineSegmentIntersection seg)
  , NumType seg ~ NumType lineSegment
  , Dimension seg ~ Dimension lineSegment
  )
