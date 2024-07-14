{-# LANGUAGE  UndecidableInstances  #-}
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

  , bruteForceLowerEnvelope

  , VertexForm
  , computeVertexForm

  , intersectionPoint
  , intersectionLine

  ) where

import           Control.Lens
import qualified Data.List as List
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (mapMaybe)
-- import           Data.Sequence (Seq)
-- import qualified Data.Sequence as Seq
import           Data.Set (Set)
import qualified Data.Set as Set
import           HGeometry.Combinatorial.Util
-- import           HGeometry.HalfLine
import           HGeometry.HyperPlane.Class
import           HGeometry.HyperPlane.NonVertical
import           HGeometry.Intersection
import           HGeometry.Line
import           HGeometry.Line.General
import           HGeometry.Point
-- import           HGeometry.Polygon.Convex
import           HGeometry.Properties
import           HGeometry.Vector

import           Debug.Trace
--------------------------------------------------------------------------------

type CircularList a = [a]

-- | A region in the minimization diagram. The boundary is given in CCW order; i.e. the
-- region is to the left of the boundary.
data Region r point = Bounded   (CircularList point)
                    | Unbounded (Vector 2 r)
                                -- ^ vector indicating the direction of the unbounded edge
                                -- incident to the first vertex. Note that this vector
                                -- thus points INTO vertex v.
                                (NonEmpty point)
                                -- ^ the vertices in CCW order,
                                (Vector 2 r)
                                -- ^ the vector indicating the direction of the unbounded
                                -- edge incident to the last vertex. The vector points
                                -- away from the vertex (i.e. towards +infty).
                      deriving stock (Show,Eq,Functor,Foldable,Traversable)


type instance NumType   (Region r point) = r
type instance Dimension (Region r point) = Dimension point

type MinimizationDiagram r plane = Map plane (Region r (Point 2 r))
-- every plane produces at most one region.

type instance NumType   (MinimizationDiagram r plane) = r
type instance Dimension (MinimizationDiagram r plane) = 2

--------------------------------------------------------------------------------
-- * The naive O(n^4) time algorithm.

type VertexForm r plane = Map (Point 3 r) (Set plane)


-- | Computes the lower envelope in O(n^4) time.
bruteForceLowerEnvelope :: ( Plane_ plane r, Ord plane, Ord r, Fractional r
                           , Foldable set
                          , Show r, Show plane

                           ) => set plane -> MinimizationDiagram r plane
bruteForceLowerEnvelope = fromVertexForm . computeVertexForm

-- | Computes the vertices of the lower envelope
--
-- O(n^4) time.
computeVertexForm        :: (Plane_ plane r, Ord plane, Ord r, Fractional r, Foldable set)
                         => set plane -> VertexForm r plane
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
fromVertexForm :: (Plane_ plane r, Ord plane, Ord r, Fractional r
                          , Show r, Show plane

                  )
               => VertexForm r plane -> MinimizationDiagram r plane
fromVertexForm = Map.mapWithKey sortAroundBoundary . mapWithKeyMerge (\v defs ->
                    Map.fromSet (const $ Set.singleton (v, defs)) defs)
-- for each vertex v, we go through its definers defs; and for each such plane h, we
-- associate it with with the set {(v,defs)}. the foldMapWithKey part thus collects all
-- vertices (together with their definers) incident to h. i.e. it combines these sets {(v,
-- defsV)} and {(u, defsU)} etc into one big set with all vertices.

--FIXME: this uses the wrong unioning; since <> for Map apparently just prefers the left value


newtype MergingMap k v = MergingMap { getMap :: Map k v }

instance (Ord k, Semigroup v) => Semigroup (MergingMap k v) where
  (MergingMap ma) <> (MergingMap mb) = MergingMap $ Map.unionWith (<>) ma mb

instance (Ord k, Semigroup v) => Monoid (MergingMap k v) where
  mempty = MergingMap mempty

mapWithKeyMerge   :: (Ord k, Ord k', Semigroup v')
                  => (k -> v -> Map k' v') -> Map k v -> Map k' v'
mapWithKeyMerge f = getMap . Map.foldMapWithKey (\k v -> MergingMap $ f k v)



-- | Given a plane h, and the set of vertices incident to h, compute the corresponding
-- region in the minimization diagram.
sortAroundBoundary            :: (Plane_ plane r, Ord r, Fractional r, Ord plane
                          , Show r, Show plane

                                 )
                              => plane -> Set (Point 3 r, Set plane) -> Region r (Point 2 r)
sortAroundBoundary h vertices = case map project (Set.toList vertices) of
    []              -> error "absurd: every plane has a non-empty set of incident vertices"
    [v]             -> let (u,p,w) = singleVertex h v in Unbounded u (NonEmpty.singleton p) w
    verts@((p,_):_) -> let vertices' = sortCCWAround p verts
                           edges     = zip vertices' (drop 1 vertices' <> vertices')
                       in case List.break (isInvalid h) edges of
                            (vs, (u,v) : ws) -> let chain = NonEmpty.fromList . map (fst . fst)
                                                          $ ws <> vs <> [(u,v)]
                                                in traceShowWith (h,"invalidEdge",u,v,) $
                                                  unboundedRegion h chain v u
                            (_,  [])         -> Bounded $ map fst vertices'

-- | Given a plane h, and the single vertex v incident to the region of h, computes the
-- unbounded region for h.
--
-- In particular, returns the pair (u,p,w). h is the region to the left of u, and also the
-- left of w.
singleVertex           :: (Plane_ plane r, Ord r, Fractional r, Ord plane
                          , Show r, Show plane
                          )
                       => plane -> (Point 2 r, Set plane) -> (Vector 2 r, Point 2 r, Vector 2 r)
singleVertex h (v,defs) = case mapMaybe withIntersectionLine . Set.toList $ Set.delete h defs of
      (h1:h2:_) -> traceShowWith (h,"result",) $
        case traceShowWith (h,) $ sortBySlope (h `onSideOf` h1,  h `onSideOf` h2) of
                     (Left w1,  Left w2)  -> (w1,        v,w2)
                     (Left w1,  Right w2) -> (negated w2,v,w1)
                     (Right w1, Left w2)  -> (w2        ,v,negated w1)
                     (Right w1, Right w2) -> (negated w1,v,negated w2)
      _         -> error "absurd: too few planes"
  where
    withIntersectionLine h' = (\w -> (h', LinePV v w)) <$> intersectionVec h h'

    sortBySlope t@(a, b)
      | slope' a <= slope' b = t
      | otherwise            = (b,a)
      where
        slope' = either id id
    -- we make sure the vector w1 is less steep than w2.


-- | Given:
--
-- - plane h bounding an unbounded region R
-- - the chain of vertices bounding R
-- - the first vertex v (with its definers)
-- - the last vertex v (with its definers)
--
-- computes the actual region R. In particular, we compute the direction that the
-- unbounded edges have.
unboundedRegion              :: (Plane_ plane r, Ord r, Fractional r, Ord plane
                          , Show r, Show plane

                                )
                             => plane
                             -> NonEmpty (Point 2 r)
                             -> (Point 2 r, Set plane) -> (Point 2 r, Set plane)
                             -> Region r (Point 2 r)
unboundedRegion h chain v@(v',_) u@(u',_)  = Unbounded wv chain wu
  where
    (v1,_,v2) = traceShowWith (h,"V",v',"->",) $ singleVertex h v
    (u1,_,u2) = traceShowWith (h,"U",u',"->",) $ singleVertex h u

    wv = case ccwCmpAroundWith (u' .-. v') origin (Point $ negated v1) (Point $ negated v2) of
           LT -> v1
           EQ -> v2 -- this probably shouldn't happen
           GT -> v2

      -- | ccw (v' .-^ v1) v' u' == CCW = v1
      --  | otherwise                    = v2

    wu = case ccwCmpAroundWith (u' .-. v') origin (Point u1) (Point u2) of
           LT -> u2
           EQ -> u1 -- this probably shouldn't happen
           GT -> u1
    -- we essentially compare the vectors u1 and u2, and pick the "most CCW" one with respect
    -- to the vector from v to u.


      -- | ccw v' u' (u' .+^ u1) == CCW = u1
      --  | otherwise                    = u2




    -- TODO: update doc

  -- overall idea: we reduce to the single vertex case; essentially computing the
  -- region of h in the minimization diagram with respect to just the definers of v.
  -- this region has two unbounded edges that have directions v1 and v2.
  --
  -- the direction wv of the unbounded edge incident to v is one of these two vectors;
  -- either v1 or v2. In particular, it is the direction so that if we go from u to v
  -- we make a right-turn (= clockwise turn).
  --
  -- We symettrically compute the diagram around u, giving us two candidates u1 and
  -- u2. Going from v to u we have to make a CCW turn, so we pick that direction as wu.


-- | given: a plane h, and (h',l) where l is the intersection line of h and h'.  computes
-- the side of l on which h is the lowest. (In both cases we tag it with the vector
-- corresponding of the line.)
--
onSideOf :: (Plane_ plane r, Ord r, Num r)
         => plane -> (plane, LinePV 2 r) -> Either (Vector 2 r) (Vector 2 r)
h `onSideOf` (h', LinePV p v)
    | evalAt q h <= evalAt q h' = Right v
    | otherwise                 = Left v
  where
    q = p .+^ rot90Cw v -- q should be a point in the right halfplane of l.


-- | Returns a vector indicating the intersection direction
intersectionVec      :: ( Plane_ plane r, Ord r, Fractional r)
                     => plane -> plane -> Maybe (Vector 2 r)
intersectionVec h h' = intersectionLine h h' <&> \case
    VerticalLineThrough _    -> Vector2 0 1
    NonVertical (LineEQ a _) -> Vector2 1 a

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

sortCCWAround   :: (Ord r, Num r) => Point 2 r -> [(Point 2 r, a)] -> [(Point 2 r, a)]
sortCCWAround c = List.sortBy (\(p,_) (q,_) -> ccwCmpAround c p q <> cmpByDistanceTo c p q)
