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
  , mergeDefiners

  , VertexForm
  ) where

import qualified Data.Foldable as F
import qualified Data.List as List
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe, listToMaybe)
import           Data.Set (Set)
import qualified Data.Set as Set
import           HGeometry.Combinatorial.Util
import           HGeometry.HyperPlane.Class
import           HGeometry.HyperPlane.NonVertical
import           HGeometry.Intersection
import           HGeometry.Line
import           HGeometry.Point
import           HGeometry.Properties
import           HGeometry.Vector
import           HGeometry.Plane.LowerEnvelope.Connected.MonoidalMap
import           HGeometry.Plane.LowerEnvelope.Connected.Primitives

--------------------------------------------------------------------------------
-- * The Minimization Diagram, i.e. the type that we use to represent our
-- lower envelope

-- | A minimization daigram just maps every plane on the lower envelope to the region
-- above which it is minimal. Every plane has at most one such a region.
type MinimizationDiagram r plane = Map plane (Region r (Point 2 r))

type instance NumType   (MinimizationDiagram r plane) = r
type instance Dimension (MinimizationDiagram r plane) = 2
-- TODO: this is a bit of hack; maybe use a newtype instead. We only need these instances
-- to actually print the thing in ipe.

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

-- | bounded regions are really circular lists, but we just represent them as lists for
-- now.
type CircularList a = [a]


--------------------------------------------------------------------------------
-- *  The planes defining a vertex

-- | The vertices of a lower envelope is just a Map with every vertex its definers,
-- i.e. the planes that define the vertex in CCW order around it.
type VertexForm r plane = Map (Point 3 r) (Definers plane)

----------------------------------------
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

             vMinH  = fromMaybe err $ intersectionVector h  hMin
             vMinH' = fromMaybe err $ intersectionVector h' hMin
             err = error "definers: absurd"

-- | given three elements, returns the minimum element in the first argument and the
-- remaining two elements in the second and third argument (in arbitrary order).
extractMinOn         :: Ord a => (c -> a) -> c -> c -> c -> (c, c, c)
extractMinOn f a b c = let (m,ab)  = min' a b
                           (mi,c') = min' m c
                       in (mi, ab, c')
  where
    min' x y
      | f x <= f y = (x,y)
      | otherwise  = (y,x)

----------------------------------------

-- | returns the CCW predecessor, and CCW successor of the given plane.
findNeighbours                   :: Eq plane => plane -> Definers plane -> Maybe (plane,plane)
findNeighbours h (Definers defs) = withNeighs <$> List.elemIndex h defs
  where
    withNeighs i = (defs List.!! ((i-1) `mod` k), defs List.!! ((i+1) `mod` k))
    k = length defs
-- TODO not exactly the best implementation yet


-- | Adds a plane to the definers. In particular, places it in the right cyclic order.
-- pre: the plane is an actual definer; i.e. it also goes through the vertex.
insertPlane :: (Plane_ plane r, Eq plane, Ord r, Fractional r)
            => Point 2 r -- ^ the projection of the vertex
            -> plane     -- ^ the plane that is a definer
            -> Definers plane -> Definers plane
insertPlane (Point2 x y) plane (Definers planes) = Definers $ go plane planes
  where
    q  = Point2 x     (y+1)
    q' = Point2 (x-1) (y+1)
    up = Vector2 0 1

    -- We first test if the new plane is the new first plane (i.e. that is minimal just
    -- above the vertex.)
    go h hs = case hs of
      []                   -> [h] -- this case should never happen really
      (h':hs') | h == h'   -> hs
               | otherwise -> case evalAt q h `compare` evalAt q h' of
                                LT -> h : insert  h  h' hs' h
                                EQ -> case evalAt q' h `compare` evalAt q' h' of
                                        LT -> h  : insert h  h' hs' h
                                        _  -> h' : insert h' h  hs' h'
                                        -- h and h' cannot both be equal at v, q and q'
                                        -- unless they are the same plane.
                                GT -> h' : insert h' h  hs' h'

    -- | try to insert h into the cyclic order.
    -- pre: hPrev is guaranteed to be in the output, and occurs before h and hNxt
    --      h0 is the final plane that also started the cyclic order.
    -- hs are the remaining planes in the yclic order.
    insert hPrev h hs h0 = case hs of
      [] -> case ccwCmpAroundWith up origin (Point u) (Point w) of
              GT -> [h] -- insert h and finish
              _  -> [] -- h is dominated; don't insert it
        where
          u  = fromMaybe err $ intersectionVector h0 hPrev
          w  = fromMaybe err $ intersectionVector h  h0
          err = error "insertPlane: insert basecase. absurd"
      (hNxt:hs')
        | h == hNxt  -> hs -- duplicate plane; exit early
        | otherwise  -> case ccwCmpAroundWith up origin (Point u) (Point w) of
          GT -> h : finish h hs h0
          -- we've inserted h, so drop the planes that are dominated by h
          _  -> hNxt : insert hNxt h hs' h0 -- try to insert at a later position
        where
          u  = fromMaybe err $ intersectionVector hNxt hPrev
          w  = fromMaybe err $ intersectionVector h    hNxt
          err = error "insertPlane: insert. absurd"

    -- we've just inserted hNew, so see if we still need/want the remaining planes
    -- this is essentially List.dropWhile, but we may need the h0 plane in the end.
    finish hNew hs h0 = case hs of
      []      -> []
      (h:hs') -> case ccwCmpAroundWith up origin (Point u) (Point w) of
                   LT -> hs  -- hNew's region indeed ends before h's region ends, so keep
                             -- h and the remaining planes
                   _  -> finish hNew hs' h0 -- hNew dominates h, so drop h.
        where
          hNxt = fromMaybe h0 $ listToMaybe hs'

          u  = fromMaybe err $ intersectionVector hNxt hNew
          w  = fromMaybe err $ intersectionVector h    hNxt
          err = error "insertPlane: finish. absurd"

-- | Merge two lists of definers.
--
-- O(n^2), since we are using incremental insertion.
mergeDefiners                :: (Plane_ plane r, Eq plane, Ord r, Fractional r)
                             => Point 3 r
                             -> Definers plane -> Definers plane
                             -> Definers plane
mergeDefiners (Point3 x y _) = foldr (insertPlane $ Point2 x y)
-- TODO: improve running time

--------------------------------------------------------------------------------
-- * Converting into a minimization diagram

-- | Given the vertices of the lower envelope; compute the minimization diagram.
--
-- \(O(h\log h)\) assuming that the input is degenerate.
fromVertexForm :: (Plane_ plane r, Ord plane, Ord r, Fractional r)
               => VertexForm r plane -> MinimizationDiagram r plane
fromVertexForm = Map.mapWithKey sortAroundBoundary . mapWithKeyMerge (\v defs ->
                    Map.fromList [ (h, Set.singleton (v,defs))
                                 | h <- F.toList defs
                                 ])
-- for each vertex v, we go through its definers defs; and for each such plane h, we
-- associate it with with the set {(v,defs)}. the foldMapWithKey part thus collects all
-- vertices (together with their definers) incident to h. i.e. it combines these sets {(v,
-- defsV)} and {(u, defsU)} etc into one big set with all vertices.

-- TODO: we should improve the running time of merge for the definers from O(k^2) to
-- O(k\log k). that should bring the overall running time to O(nlog n) even for degenerate
-- inputs.

-- | Given a plane h, and the set of vertices incident to h, compute the corresponding
-- region in the minimization diagram.
sortAroundBoundary            :: (Plane_ plane r, Ord r, Fractional r, Ord plane)
                              => plane -> Set (Point 3 r, Definers plane)
                              -> Region r (Point 2 r)
sortAroundBoundary h vertices = case inCCWOrder . map project . Set.toList $ vertices of
    []        -> error "absurd: every plane has a non-empty set of incident vertices"
    [v]       -> let (u,p,w) = singleVertex h v in Unbounded u (NonEmpty.singleton p) w
    vertices' -> let edges     = zip vertices' (drop 1 vertices' <> vertices')
                 in case List.break (isInvalid h) edges of
                            (vs, (u,v) : ws) -> let chain = NonEmpty.fromList . map (fst . fst)
                                                          $ ws <> vs <> [(u,v)]
                                                in unboundedRegion h chain v u
                            (_,  [])         -> Bounded $ map fst vertices'


-- | Given a plane h, and the single vertex v incident to the region of h, computes the
-- unbounded region for h.
--
-- In particular, returns the pair (u,p,w). h is the region to the left of u, and also the
-- left of w.
singleVertex           :: (Plane_ plane r, Ord r, Fractional r, Ord plane)
                       => plane -> (Point 2 r, Definers plane)
                       -> (Vector 2 r, Point 2 r, Vector 2 r)
singleVertex h (v,defs) = fromMaybe (error "singleVertex: absurd") $
    do (hPred,hSucc) <- findNeighbours h defs
       u             <- intersectionVector h hPred
       w             <- intersectionVector h hSucc
       pure (w, v, u)

-- | Given:
--
-- - plane h bounding an unbounded region R
-- - the chain of vertices bounding R
-- - the first vertex v (with its definers)
-- - the last vertex v (with its definers)
--
-- computes the actual region R. In particular, we compute the direction that the
-- unbounded edges have.
unboundedRegion              :: (Plane_ plane r, Ord r, Fractional r, Ord plane)
                             => plane
                             -> NonEmpty (Point 2 r)
                             -> (Point 2 r, Definers plane) -> (Point 2 r, Definers plane)
                             -> Region r (Point 2 r)
unboundedRegion h chain v@(v',_) u@(u',_)  = Unbounded wv chain wu
  where
    (v1,_,v2) = singleVertex h v
    (u1,_,u2) = singleVertex h u

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

-- | Project the vertex onto the plane.
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
