--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Plane.LowerEnvelope.Regions
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- A Representation of the Lower envelope of planes as a bunch of convex regions
--
--------------------------------------------------------------------------------
module HGeometry.Plane.LowerEnvelope.Connected.Regions
  ( MinimizationDiagram
  , Region(..)
  , CircularList
  , fromVertexForm

  , VertexForm
  , computeVertexForm

  , intersectionPoint
  , intersectionLine
  ) where

import qualified Data.List as List
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Data.Set (Set)
import qualified Data.Set as Set
import           HGeometry.Combinatorial.Util
import           HGeometry.HalfLine
import           HGeometry.HyperPlane.Class
import           HGeometry.HyperPlane.NonVertical
import           HGeometry.Intersection
import           HGeometry.Line
import           HGeometry.Line.General
import           HGeometry.Point
import           HGeometry.Polygon.Convex
import           HGeometry.Properties
import           HGeometry.Vector

--------------------------------------------------------------------------------

type CircularList a = [a]

data Region point = Bounded   (CircularList point)
                  | Unbounded (HalfLine point)
                              [point]
                    -- in CCW order, includes the starting points of the
                    -- halflines (which may be the same anyway.)
                              (HalfLine point)

-- deriving instance (Eq point, Eq (NumType point), Dimension point ~ 2)   => Eq (Region point)
-- deriving instance (Show point, Show (NumType point), Point_ point 2 r) => Show (Region point)


type MinimizationDiagram r plane = Map plane (Region (Point 2 r))
-- every plane produces at most one region.

--------------------------------------------------------------------------------
-- * The naive O(n^4) time algorithm.

type VertexForm r plane = Map (Point 3 r) (Set plane)

computeVertexForm        :: (Plane_ plane r, Ord plane, Ord r, Fractional r, Foldable f)
                         => f plane -> VertexForm r plane
computeVertexForm planes = foldMap (asVertex planes) $ uniqueTriplets planes

asVertex             :: (Plane_ plane r, Foldable f, Ord plane, Ord r, Fractional r)
                     => f plane -> Three plane -> Map (Point 3 r) (Set plane)
asVertex planes defs = case intersectionPoint defs of
  Just v  | v `belowAll` planes -> Map.singleton v (foldMap Set.singleton defs)
  _                             -> Map.empty


-- | test if v lies below (or on) all the given planes
belowAll   :: (Plane_ plane r, Ord r, Num r, Foldable f) => Point 3 r -> f plane -> Bool
belowAll v = all (\h -> onSideTest v h /= GT)
{-# INLINE belowAll #-}

-- | Given two planes, computes the line in which they intersect.
intersectionLine :: (Plane_ plane r, Fractional r, Eq r)
                 => plane -> plane -> Maybe (VerticalOrLineEQ r)
intersectionLine (Plane_ a1 b1 c1) (Plane_ a2 b2 c2)
    | b1 /= b2  = Just $ NonVertical $ LineEQ ((a2 - a1) / diffB) ((c2 - c1) / diffB)
                  -- the two planes intersect in some normal line
    | a1 /= a2  = Just $ VerticalLineThrough ((c2 -c1) / (a1 - a2))
                  -- the planes intersect in a vertical line
    | otherwise = Nothing
                  -- the planes don't intersect at all
  where
    diffB = b1 - b2

-- | Computes there the three planes intersect
intersectionPoint                                    :: ( Plane_ plane r, Ord r, Fractional r)
                                                     => Three plane -> Maybe (Point 3 r)
intersectionPoint (Three h1@(Plane_ a1 b1 c1) h2 h3) =
    do l12 <- intersectionLine h1 h2
       l13 <- intersectionLine h1 h3
       case (l12,l13) of
         (VerticalLineThrough _x12, VerticalLineThrough _x13) -> Nothing
           -- if the xes are the same they would be the same plane even
         (VerticalLineThrough x, NonVertical l)               -> vertNonVertIntersect x l
         (NonVertical l, VerticalLineThrough x)               -> vertNonVertIntersect x l
         (NonVertical l, NonVertical m)                       -> l `intersect` m >>= \case
           Line_x_Line_Point (Point2 x y) -> Just $ Point3 x y (a1 * x + b1* y + c1)
           Line_x_Line_Line _             -> Nothing
   where
     vertNonVertIntersect x l = let y = evalAt' x l
                                    z = a1 * x + b1* y + c1
                                in Just $ Point3 x y z

--------------------------------------------------------------------------------
-- * Converting into a minimization diagram

-- | Given the vertices of the lower envelope; compute the minimization diagram.
--
--
fromVertexForm :: (Plane_ plane r, Ord plane, Ord r, Fractional r)
               => VertexForm r plane -> MinimizationDiagram r plane
fromVertexForm = Map.mapWithKey sortAroundBoundary . Map.foldMapWithKey (\v defs ->
                    Map.fromSet (const $ Set.singleton (v, defs)) defs)
-- for each vertex v, we go through its definers defs; and for each such plane h, we
-- associate it with with the set {(v,defs)}. the foldMapWithKey part thus collects all
-- vertices (together with their definers) incident to h. i.e. it combines these sets {(v,
-- defsV)} and {(u, defsU)} etc into one big set with all vertices.


-- | Given a plane h, and the set of vertices incident to h, compute the corresponding
-- region in the minimization diagram.
sortAroundBoundary            :: (Plane_ plane r, Ord r, Fractional r, Ord plane)
                              => plane -> Set (Point 3 r, Set plane) -> Region (Point 2 r)
sortAroundBoundary h vertices = case map project (Set.toList vertices) of
    []                    -> error "absurd: every plane has a non-empty set of incident vertices"
    [(v,defsV)]           -> Unbounded undefined [v] undefined
    [(u,defsU),(v,defsV)] -> Unbounded undefined [u, v]  undefined
    vs@((p,_):_)          -> let vertices' = sortAround' p vs
                                 edges     = zip vertices' (drop 1 vertices' <> vertices')
                                 f         = fst . fst
                             in case List.break (isInvalid h) edges of
                                  (vs, (u,v) : ws) -> Unbounded undefined (map f $ ws <> vs) undefined
                                  (_,  [])         -> Bounded $ map fst vertices'


-- | Test if (u,v) is a valid edge bounding the region of h.
isInvalid                          :: (Plane_ plane r, Ord r, Fractional r)
                                   => plane
                                   -> ((Point 2 r, Set plane), (Point 2 r, Set plane)) -> Bool
isInvalid h ((u,defsU), (v,defsV)) = all hClosest defsU && all hClosest defsV
  where
    w           = (v .-. u) ^/ 2         -- vector from u to the midpoint m of uv
    v'          = u .+^ w .+^ rot90Cw w  -- u .+^ w is midpoint,
    hClosest h' = evalAt v' h <= evalAt v' h'
    -- we essentially rotate the directed segment uv CW by 90 degrees around its
    -- center. Let u'v' denote the resulting segment. Since uv is in CCW order the point u'
    -- (which lies thus left of uv) lies in the region of h. However, if v' also closer
    -- to h than to the other planes then uv was not a real edge bounding the region of h.

rot90Cw                 :: Num r => Vector 2 r -> Vector 2 r
rot90Cw (Vector2 vx vy) = Vector2 vy (-vx)

project                     :: (Point 3 r, a) -> (Point 2 r, a)
project (Point3 x y _, loc) = (Point2 x y, loc)

sortAround'   :: (Ord r, Num r) => Point 2 r -> [(Point 2 r, a)] -> [(Point 2 r, a)]
sortAround' c = List.sortBy (\(p,_) (q,_) -> ccwCmpAround c p q <> cmpByDistanceTo c p q)
