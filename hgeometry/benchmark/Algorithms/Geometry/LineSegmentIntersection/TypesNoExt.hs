{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Algorithms.Geometry.LineSegmentIntersection.Types
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--------------------------------------------------------------------------------
module Algorithms.Geometry.LineSegmentIntersection.TypesNoExt where

-- import           Algorithms.DivideAndConquer
import           Control.DeepSeq
import           Control.Lens
import           Data.Ext
import           Data.Bifunctor
import           Data.Geometry.Interval
import           Data.Geometry.LineSegment
import           Data.Geometry.Point
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Data.Ord (comparing, Down(..))
import           GHC.Generics
import           Data.Vinyl.CoRec
import           Data.Vinyl
import           Data.Intersection


----------------------------------------------------------------------------------


-- FIXME: What do we do when one segmnet lies *on* the other one. For
-- the short segment it should be an "around start", but then the
-- startpoints do not match.
--
-- for the long one it's an "on" segment, but they do not intersect


-- | Assumes that two segments have the same start point
newtype AroundStart a = AroundStart a deriving (Show,Read,NFData)

instance Eq r => Eq (AroundStart (LineSegment 2 p r)) where
  -- | equality on endpoint
  (AroundStart s) == (AroundStart s') = s^.end.core == s'^.end.core

instance (Ord r, Num r) => Ord (AroundStart (LineSegment 2 p r)) where
  -- | ccw ordered around their suposed common startpoint
  (AroundStart s) `compare` (AroundStart s') =
    ccwCmpAround (s^.start.core) (s^.end.core)  (s'^.end.core)

----------------------------------------

-- | Assumes that two segments have the same end point
newtype AroundEnd a = AroundEnd a deriving (Show,Read,NFData)

instance Eq r => Eq (AroundEnd (LineSegment 2 p r)) where
  -- | equality on endpoint
  (AroundEnd s) == (AroundEnd s') = s^.start.core == s'^.start.core

instance (Ord r, Num r) => Ord (AroundEnd (LineSegment 2 p r)) where
  -- | ccw ordered around their suposed common end point
  (AroundEnd s) `compare` (AroundEnd s') =
    ccwCmpAround (s^.end.core) (s^.start.core)  (s'^.start.core)

--------------------------------------------------------------------------------

-- | Assumes that two segments intersect in a single point.
newtype AroundIntersection a = AroundIntersection a deriving (Show,Read,NFData)

instance Eq r => Eq (AroundIntersection (LineSegment 2 p r)) where
  -- | equality ignores the p type
  (AroundIntersection s) == (AroundIntersection s') = first (const ()) s == first (const ()) s'

instance (Ord r, Fractional r) => Ord (AroundIntersection (LineSegment 2 p r)) where
  -- | ccw ordered around their common intersection point.
  l@(AroundIntersection s) `compare` r@(AroundIntersection s') = match (s `intersect` s') $
        H (\NoIntersection     -> error "AroundIntersection: segments do not intersect!")
     :& H (\p                  -> cmpAroundP p s s')
     :& H (\_                  -> (squaredLength s) `compare` (squaredLength s'))
                                 -- if s and s' just happen to be the same length but
                                 -- intersect in different behaviour from using (==).
                                 -- but that situation doese not satisfy the precondition
                                 -- of aroundIntersection anyway.
     :& RNil
    where
      squaredLength (LineSegment' a b) = squaredEuclideanDist (a^.core) (b^.core)

-- | compare around p
cmpAroundP        :: (Ord r, Num r) => Point 2 r -> LineSegment 2 p r -> LineSegment 2 p r -> Ordering
cmpAroundP p s s' = ccwCmpAround p (s^.start.core)  (s'^.start.core)


-- seg1 = ClosedLineSegment (ext $ Point2 0 0) (ext $ Point2 0 10)
-- seg2 = ClosedLineSegment (ext $ Point2 0 0) (ext $ Point2 0 10)


--------------------------------------------------------------------------------



-- | The line segments that contain a given point p may either have p
-- as the endpoint or have p in their interior.
--
-- if somehow the segment is degenerate, and p is both the start and
-- end it is reported only as the start point.
data Associated p r =
  Associated { _startPointOf :: Set.Set (AroundEnd (LineSegment 2 p r))
             -- ^ segments for which the intersection point is the
             -- start point (i.e. s^.start.core == p)
             , _endPointOf   :: Set.Set (AroundStart (LineSegment 2 p r))
             -- ^ segments for which the intersection point is the end
             -- point (i.e. s^.end.core == p)
             , _interiorTo   :: Set.Set (AroundIntersection (LineSegment 2 p r))
             } deriving stock (Show, Read, Generic, Eq)

makeLenses ''Associated



-- | Reports whether this associated has any interior intersections
--
-- \(O(1)\)
isInteriorIntersection :: Associated p r -> Bool
isInteriorIntersection = not . null . _interiorTo


-- | test if the given segment has p as its endpoint, an construct the
-- appropriate associated representing that.
--
-- pre: p intersects the segment
mkAssociated                :: (Ord r, Fractional r)
                            => Point 2 r -> LineSegment 2 p r -> Associated p r
mkAssociated p s@(LineSegment a b)
  | p == a^.unEndPoint.core = mempty&startPointOf .~  Set.singleton (AroundEnd s)
  | p == b^.unEndPoint.core = mempty&endPointOf   .~  Set.singleton (AroundStart s)
  | otherwise               = mempty&interiorTo   .~  Set.singleton (AroundIntersection s)


-- | test if the given segment has p as its endpoint, an construct the
-- appropriate associated representing that.
--
-- If p is not one of the endpoints we concstruct an empty Associated!
--
mkAssociated'     :: (Ord r, Fractional r) => Point 2 r -> LineSegment 2 p r -> Associated p r
mkAssociated' p s = (mkAssociated p s)&interiorTo .~ mempty

instance (Ord r, Fractional r) => Semigroup (Associated p r) where
  (Associated ss es is) <> (Associated ss' es' is') =
    Associated (ss <> ss') (es <> es') (is <> is')

instance (Ord r, Fractional r) => Monoid (Associated p r) where
  mempty = Associated mempty mempty mempty

instance (NFData p, NFData r) => NFData (Associated p r)

-- | For each intersection point the segments intersecting there.
type Intersections p r = Map.Map (Point 2 r) (Associated p r)

-- | An intersection point together with all segments intersecting at
-- this point.
data IntersectionPoint p r =
  IntersectionPoint { _intersectionPoint :: !(Point 2 r)
                    , _associatedSegs    :: !(Associated p r)
                    } deriving (Show,Read,Eq,Generic)
makeLenses ''IntersectionPoint

instance (NFData p, NFData r) => NFData (IntersectionPoint p r)


-- sameOrder           :: (Ord r, Num r, Eq p) => Point 2 r
--                     -> [LineSegment 2 p r] -> [LineSegment 2 p r] -> Bool
-- sameOrder c ss ss' = f ss == f ss'
--   where
--     f = map (^.extra) . sortAround' (ext c) . map (\s -> s^.end.core :+ s)




-- | Given a point p, and a bunch of segments that suposedly intersect
-- at p, correctly categorize them.
mkIntersectionPoint         :: (Ord r, Fractional r)
                            => Point 2 r
                            -> [LineSegment 2 p r] -- ^ uncategorized
                            -> [LineSegment 2 p r] -- ^ segments we know contain p,
                            -> IntersectionPoint p r
mkIntersectionPoint p as cs = IntersectionPoint p $ foldMap (mkAssociated p) $ as <> cs

  -- IntersectionPoint p
  --                           $ Associated mempty mempty (Set.fromAscList cs')
  --                           <> foldMap (mkAssociated p) as
  -- where
  --   cs' = map AroundIntersection . List.sortBy (cmpAroundP p) $ cs
  -- -- TODO: In the bentley ottman algo we already know the sorted order of the segments
  -- -- so we can likely save the additional sort



-- | An ordering that is decreasing on y, increasing on x
ordPoints     :: Ord r => Point 2 r -> Point 2 r -> Ordering
ordPoints a b = let f p = (Down $ p^.yCoord, p^.xCoord) in comparing f a b
