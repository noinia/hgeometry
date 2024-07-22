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

  , Definers
  , fromCCWList
  , definers


  , bruteForceLowerEnvelope

  , VertexForm
  , computeVertexForm

  , intersectionPoint
  , intersectionLine

  ) where

import           Control.Lens
import qualified Data.Foldable as F
import qualified Data.List as List
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
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
-- *  The planes defining a vertex

-- | in CCW order, starting with the plane that is minimal at the vertical up direction
-- from their common vertex.
newtype Definers plane = Definers [plane]
  deriving stock (Show,Eq,Ord)
  deriving newtype (Functor,Foldable)

-- | Given the planes in order, starting with the one that is closest in the up direction,
-- construct the Definers.
fromCCWList :: [plane] -> Definers plane
fromCCWList = Definers

-- | Smart constructor for creating the definers of three planes
definers                                    :: (Plane_ plane r, Ord r, Fractional r)
                                            => Three plane -> Maybe (Point 3 r, Definers plane)
definers (Three h1@(Plane_ a1 b1 c1) h2 h3) =
    do l12 <- intersectionLine h1 h2
       l13 <- intersectionLine h1 h3
       intersect l12 l13 >>= \case
         Line_x_Line_Line _             -> Nothing
         Line_x_Line_Point (Point2 x y) -> Just ( Point3 x y (a1 * x + b1* y + c1)
                                                , Definers [hMin, hTwo, hThree]
                                                )
           where
             (hMin,h,h')   = extractMinOn (evalAt $ Point2 x (y+1)) h1 h2 h3
             -- we compute the plane hMin that is cheapest directly above the vertex h and
             -- h' are the other two planes. That Means hMin is the first definer (in the
             -- CCW order). What remains is to determine the order in which the remaining
             -- planes h and h' appear in the order.
             (hTwo,hThree) = case ccwCmpAroundWith (Vector2 0 1) origin
                                                   (Point vMinH) (Point vMinH') of
                               LT -> (h,h')
                               EQ -> error "definers: weird degeneracy?"
                               GT -> (h',h)

             LinePV _ vMinH  = fromMaybe err $ intersectionLine' h  hMin
             LinePV _ vMinH' = fromMaybe err $ intersectionLine' h' hMin
             err = error "definers: absurd"

extractMinOn         :: Ord a => (c -> a) -> c -> c -> c -> (c, c, c)
extractMinOn f a b c = let (m,ab)  = min' a b
                           (mi,c') = min' m c
                       in (mi, ab, c')
  where
    min' x y
      | f x <= f y = (x,y)
      | otherwise  = (y,x)

-- | returns the CCW predecessor, and CCW successor of the given plane.
findNeighbours                   :: Eq plane => plane -> Definers plane -> Maybe (plane,plane)
findNeighbours h (Definers defs) = withNeighs <$> List.elemIndex h defs
  where
    withNeighs i = (defs List.!! ((i-1) `mod` k), defs List.!! ((i+1) `mod` k))
    k = length defs
-- not exactly the best implementation yet

instance Semigroup (Definers plane) where
  _defsA <> _defsB = error "semigroup for definers not implemented yet"

--------------------------------------------------------------------------------
-- * The naive O(n^4) time algorithm.

type VertexForm r plane = Map (Point 3 r) (Definers plane)


-- | Computes the lower envelope in O(n^4) time.
bruteForceLowerEnvelope :: ( Plane_ plane r, Ord plane, Ord r, Fractional r
                           , Foldable set
                           , Show r, Show plane

                           ) => set plane -> MinimizationDiagram r plane
bruteForceLowerEnvelope = fromVertexForm . computeVertexForm

-- | Computes the vertices of the lower envelope
--
-- O(n^4) time.
computeVertexForm        :: (Plane_ plane r, Ord plane, Ord r, Fractional r, Foldable set
                            , Show plane, Show r
                            )
                         => set plane -> VertexForm r plane
computeVertexForm planes = getMap . foldMap (asVertex planes) $ uniqueTriplets planes

asVertex             :: (Plane_ plane r, Foldable f, Ord plane, Ord r, Fractional r
                        , Show plane, Show r
                        )
                     => f plane -> Three plane -> MonoidalMap (Point 3 r) (Definers plane)
asVertex planes defs = case definers defs of
  Just (v,defs')  | v `belowAll` planes ->traceShowWith ("vertex",) $
                    MonoidalMap (Map.singleton v defs')
  _                                     -> MonoidalMap Map.empty

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

-- | Computes the line in which the two planes h and h' intersect. The returned line will
-- have h to its left and h' to its right.
--
intersectionLine'      :: ( Plane_ plane r, Ord r, Fractional r)
                       => plane -> plane -> Maybe (LinePV 2 r)
intersectionLine' h h' = intersectionLine h h' <&> \case
    VerticalLineThrough x -> reorient (LinePV (Point2 x 0) (Vector2 0 1)) (Point2 (x-1) 0)
    NonVertical l         -> let l'@(LinePV p _) = fromLineEQ l
                             in reorient l' (p&yCoord %~ (+1))
  where
    -- make sure h is to the left of the line
    reorient l q = let f = evalAt q
                   in if f h <= f h' then l else l&direction %~ negated
    fromLineEQ (LineEQ a b) = fromLinearFunction a b




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
                    Map.fromList [ (h, Set.singleton (v,defs))
                                 | h <- F.toList defs
                                 ])
-- for each vertex v, we go through its definers defs; and for each such plane h, we
-- associate it with with the set {(v,defs)}. the foldMapWithKey part thus collects all
-- vertices (together with their definers) incident to h. i.e. it combines these sets {(v,
-- defsV)} and {(u, defsU)} etc into one big set with all vertices.

newtype MonoidalMap k v = MonoidalMap { getMap :: Map k v }
  deriving (Show)

instance (Ord k, Semigroup v) => Semigroup (MonoidalMap k v) where
  (MonoidalMap ma) <> (MonoidalMap mb) = MonoidalMap $ Map.unionWith (<>) ma mb

instance (Ord k, Semigroup v) => Monoid (MonoidalMap k v) where
  mempty = MonoidalMap mempty

-- | Merge the maps. When they share a key, combine their values using a semigroup.
mapWithKeyMerge   :: (Ord k, Ord k', Semigroup v')
                  => (k -> v -> Map k' v') -> Map k v -> Map k' v'
mapWithKeyMerge f = getMap . Map.foldMapWithKey (\k v -> MonoidalMap $ f k v)


-- | Given a plane h, and the set of vertices incident to h, compute the corresponding
-- region in the minimization diagram.
sortAroundBoundary            :: (Plane_ plane r, Ord r, Fractional r, Ord plane
                          , Show r, Show plane

                                 )
                              => plane -> Set (Point 3 r, Definers plane)
                              -> Region r (Point 2 r)
sortAroundBoundary h vertices = case inCCWOrder . map project . Set.toList $ vertices of
    []        -> error "absurd: every plane has a non-empty set of incident vertices"
    [v]       -> let (u,p,w) = singleVertex h v in Unbounded u (NonEmpty.singleton p) w
    vertices' -> let edges     = zip vertices' (drop 1 vertices' <> vertices')
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
                       => plane -> (Point 2 r, Definers plane) -> (Vector 2 r, Point 2 r, Vector 2 r)
singleVertex h (v,defs) = fromMaybe (error "singleVertex: absurd") $
    do (hPred,hSucc) <- findNeighbours h defs
       LinePV _ u    <- intersectionLine' h hPred
       LinePV _ w    <- intersectionLine' h hSucc
       pure $ traceShowWith ("singleVertex",h,) (w, v, u)


  -- case mapMaybe withIntersectionLine . Set.toList $ Set.delete h defs of
  --     (hA:hB:_) -> traceShowWith (h,"result",) $
  --       case traceShowWith (h,) $ sortBySlope (h `onSideOf` hA,  h `onSideOf` hB) of
  --                    (Left w1,  Left w2)  -> (w1,        v,w2)
  --                    (Left w1,  Right w2) -> (negated w2,v,w1)
  --                    (Right w1, Left w2)  -> (w2        ,v,negated w1)
  --                    (Right w1, Right w2) -> (negated w1,v,negated w2)
  --     _         -> error "absurd: too few planes"
  -- where
  --   withIntersectionLine h' = (\w -> (h', LinePV v w)) <$> intersectionVec h h'

  --   sortBySlope t@(a, b)
  --     | slope' a <= slope' b = t
  --     | otherwise            = (b,a)
  --     where
  --       slope' = either id id

    -- the overall idea is as follows: there are (at least) two other planes hA and hB
    -- that (together with h) define v. We compute the two direction-vectors corresponding
    -- to the intersection lines of hA and hB with h, and order them by slope. Let w1 be
    -- the most vector whose line has the most negative slope, and let w2 be the other direction.
    --
    -- We then compute whether h is left or right of the *directed* lines (whose
    -- directions are w1 and w2). These two directed lines produce four quadrants,
    -- for each quadrant we then explicitly give the answers so that: the boundary
    -- is traversed in CCW order, and the vectors are oriented so that h is always to the left.


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
                             -> (Point 2 r, Definers plane) -> (Point 2 r, Definers plane)
                             -> Region r (Point 2 r)
unboundedRegion h chain v@(v',_) u@(u',_)  = Unbounded wv chain wu
  where
    (v1,_,v2) = traceShowWith (h,"V",v',"->",) $ singleVertex h v
    (u1,_,u2) = traceShowWith (h,"U",u',"->",) $ singleVertex h u

    z  = u' .-. v'
    wv = case ccwCmpAroundWith z origin (Point $ negated v1) (Point $ negated v2) of
           LT -> v1
           EQ -> v2 -- this probably shouldn't happen
           GT -> v2
    wu = case ccwCmpAroundWith z origin (Point u1) (Point u2) of
           LT -> u2
           EQ -> u1 -- this probably shouldn't happen
           GT -> u1
    -- we essentially compare the vectors u1 and u2, and pick the "most CCW" one with respect
    -- to the vector from v to u.


  -- overall idea: we reduce to the single vertex case; essentially computing the
  -- region of h in the minimization diagram with respect to just the definers of u.
  -- this region has two unbounded edges that have directions u1 and u2.
  --
  -- the direction wu of the unbounded edge incident to u is one of these two vectors;
  -- either u1 or u2. In particular, if we go from v to u, then we want the "most CCW"
  -- vector among u1 and u2.
  --
  -- We symettrically compute the diagram around v, giving us two candidates v1 and v2. If
  -- we arrive at v using v1 or v2, and then turn towards u, we should make a CCW
  -- turn. So, we again order the vectors with respect to the base vector from v to u.
  --
  -- note we can create a point p so that the vector from p to v is "v1" by using p = v
  -- .+^ (negated v1). Hence the negateds. (In the end we just translate everything so
  -- that v is the origin, so then we don't need to .+^ the v's.)



-- | given: a plane h, and (h',l) where l is the intersection line of h and h'.  computes
-- the side of l on which h is the lowest. (In both cases we tag it with the vector
-- corresponding of the line.)
--
-- onSideOf :: (Plane_ plane r, Ord r, Num r)
--          => plane -> (plane, LinePV 2 r) -> Either (Vector 2 r) (Vector 2 r)
-- h `onSideOf` (h', LinePV p v)
--     | evalAt q h <= evalAt q h' = Right v
--     | otherwise                 = Left v
--   where
--     q = p .+^ rot90Cw v -- q should be a point in the right halfplane of l.


-- | Returns a vector indicating the intersection direction
intersectionVec      :: ( Plane_ plane r, Ord r, Fractional r)
                     => plane -> plane -> Maybe (Vector 2 r)
intersectionVec h h' = intersectionLine h h' <&> \case
    VerticalLineThrough _    -> Vector2 0 1
    NonVertical (LineEQ a _) -> Vector2 1 a

-- | Test if (u,v) is an invalid edge to be on the CCW boundary of h.  this can mean that
-- either (u,v) is not an actual edge (i.e. u and v) are connected to the vertex at
-- infinity. Or that the edge is in the wrong orientation
isInvalid                          :: (Plane_ plane r, Eq plane, Ord r, Fractional r)
                                   => plane
                                   -> ( (Point 2 r, Definers plane)
                                      , (Point 2 r, Definers plane)) -> Bool
isInvalid h ((_u,defsU), (_v,defsV)) =
  fromMaybe (error "isInvalid: h not found in the definers!?") $ do
      (hPredU,_) <- findNeighbours h defsU
      (_,hSuccV) <- findNeighbours h defsV
      pure $ hPredU /= hSuccV
    -- if (u,v) is actually a valid edge, i.e. it has h to its left, then the CCW
    -- successor w.r.t to v, and the CCW predecessor w.r.t u must be the same plane.
    -- if that is not the case the edge must be invalid.


project                     :: (Point 3 r, a) -> (Point 2 r, a)
project (Point3 x y _, loc) = (Point2 x y, loc)

-- | Given a list of vertices of a (possibly unbounded) convex polygonal region (in
-- arbitrary orientation), sort the vertices so that they are listed in CCW order.
inCCWOrder     :: (Ord r, Fractional r) => [(Point 2 r, a)] -> [(Point 2 r, a)]
inCCWOrder pts = case pts of
  ((p,_):(q,_):_) -> let c               = p .+^ ((q .-. p) ^/ 2)
                         cmp (a,_) (b,_) = ccwCmpAround c a b <> cmpByDistanceTo c a b
                     in List.sortBy cmp pts
  _               -> pts -- already sorted.
