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
  ( fromVertexForm

  , Definers
  , fromCCWList
  , definers
  , mergeDefiners

  , VertexForm


  , fromVertexFormIn
  , BoundedRegion


  , extraPoints
  ) where

import           Control.Lens
import           Data.Coerce
import qualified Data.Foldable as F
import           Data.Foldable1
import qualified Data.List as List
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Map.NonEmpty (NEMap)
import qualified Data.Map.NonEmpty as Map
import           Data.Maybe (fromMaybe)
import           Data.Monoid (First(..))
import           Data.Ord (comparing)
import           Data.Set.NonEmpty (NESet)
import qualified Data.Set.NonEmpty as Set
import           HGeometry.Combinatorial.Util
import           HGeometry.Ext
import           HGeometry.Foldable.Util
import           HGeometry.HalfLine
import           HGeometry.HyperPlane.Class
import           HGeometry.HyperPlane.NonVertical
import           HGeometry.Intersection
import           HGeometry.LineSegment
import           HGeometry.NonEmpty.Util
import           HGeometry.Plane.LowerEnvelope.Connected.MonoidalMap
import           HGeometry.Plane.LowerEnvelope.Connected.Primitives
import           HGeometry.Plane.LowerEnvelope.Connected.Type
import           HGeometry.Plane.LowerEnvelope.Connected.VertexForm
import           HGeometry.Point
import           HGeometry.Point.Either
import           HGeometry.Polygon.Class
import           HGeometry.Polygon.Convex
import           HGeometry.Polygon.Simple.Class
import           HGeometry.Triangle
import           HGeometry.Vector

import           Debug.Trace

----------------------------------------

-- | returns the CCW predecessor, and CCW successor of the given plane.
findNeighbours                   :: Eq plane => plane -> Definers plane -> Maybe (plane,plane)
findNeighbours h (Definers defs) = withNeighs <$> elemIndex h defs
  where
    withNeighs i = (defs NonEmpty.!! ((i-1) `mod` k), defs NonEmpty.!! ((i+1) `mod` k))
    k = length defs
    elemIndex q = List.elemIndex q . F.toList
-- TODO not exactly the best implementation yet


--------------------------------------------------------------------------------


-- | Compute the plane that is cheapest vertically above the vertex.
--
-- pre: all planes contain the given point
extractH0                   :: (Plane_ plane r, Eq plane, Ord r, Fractional r)
                            => Point 3 r -> NonEmpty plane -> (plane, [plane])
extractH0 (Point3 x y _) hs = case extractMinimaBy (comparing $ evalAt (Point2 x (y+1))) hs of
                                (h0 :| eqs) :+ rest -> (h0, (filter (/= h0) eqs) <> rest)

-- | Given three planes h0, h and h' that all intersect in a common vertex v,
-- and so that h0 is the lowest plane vertically above v, order the
-- other two planes in CCW order.
cmpPlanesAround                     :: (Plane_ plane r, Eq plane, Ord r, Fractional r
                                       , Show plane, Show r
                                       )
                                    => plane -> plane -> plane -> Ordering
cmpPlanesAround h0 h h' | h == h'   = EQ
                        | otherwise = case snd <$> definers (Three h0 h h') of
                            Just (Definers (_ :| [hPrev, _hSucc])) | h == hPrev -> LT
                                                                   | otherwise  -> GT
                            _ -> error $ "cmpPlanesAround: precondition failed"
                                         <> show (h0,h,h')
--  note that the first definer will, by definition/by precondition just be h0

-- | Merge two lists of definers.
--
-- \(O(n\log n)\), where \(n\) is the total number of planes involved.
mergeDefiners               :: (Plane_ plane r, Eq plane, Ord r, Fractional r
                               , Show plane, Show r
                               )
                            => Point 3 r
                            -> Definers plane -> Definers plane
                            -> Definers plane
mergeDefiners v defs0 defs1 = case extractH0 v (coerce defs0 <> coerce defs1) of
    (h0, planes) -> Definers . dropDuplicates $ h0 :| List.sortBy (cmpPlanesAround h0) planes
  where
    dropDuplicates = fmap NonEmpty.head . NonEmpty.group1
-- TODO: We may need some filtering pass to remove planes that are no longer definers
-- e.g. when some plane passes through v but does not actually show up on the envelope.
-- I'll leave that for later though.
--
-- The general strategy is as follows:
--
-- we find the plane h0 that is cheapest vertically above the point
--
-- we now define an ordering for any other planes h and h' as follows:
--
-- now for any two other planes h, h'
-- if  h == h then the relation is EQ
--
-- otherwise, consider the vertex v defined by
-- h, h', and h0.
--
-- we define h < h' if h appears directly after h0 in the CCW cyclic ordering
-- of v
-- and h > h' otherwise




--------------------------------------------------------------------------------
-- * Converting into a minimization diagram

type BoundedRegion r vertex corner =
  ConvexPolygonF NonEmpty (OriginalOrExtra vertex corner)

-- | Pre: the triangle is big enough to contain all vertices of the lower envelope
fromVertexFormIn     :: ( Plane_ plane r, Ord plane, Ord r, Fractional r, Show r, Show plane
                        , HasDefiners vertexData plane
                        , Point_ corner 2 r
                        , Ord vertexData -- figure out why we need this?
                        , Show r, Show corner
                        )
                     => Triangle corner
                     -> NEMap (Point 3 r) vertexData
                     -> NEMap plane (BoundedRegion r (Point 2 r :+ vertexData) (Point 2 r))
fromVertexFormIn tri = fmap (clipTo tri) . asMap . fromVertexForm

-- | pre; all bounded vertices lie inside the triangle
clipTo     :: (Point_ corner 2 r, Fractional r, Ord r
              , Show r, Show corner
              )
           => Triangle corner -> Region r (Point 2 r :+ vertexData)
           -> BoundedRegion r (Point 2 r :+ vertexData) (Point 2 r)
clipTo tri = \case
  Bounded vs          -> uncheckedFromCCWPoints $ Original <$> vs
  Unbounded u chain v -> let p        = NonEmpty.head chain
                             q        = NonEmpty.last chain
                             hp       = HalfLine (p^.asPoint) ((-1) *^ u)
                             hq       = HalfLine (q^.asPoint) v
                             extras   = extraPoints hp hq (view asPoint <$> tri)
                         in uncheckedFromCCWPoints $ (Extra <$> extras) <> (Original <$> chain)

-- | computes the extra vertices that we have to insert to make an unbounded region bounded
extraPoints            :: ( Triangle_ triangle corner, Point_ corner 2 r
                          , Point_ point 2 r, Fractional r, Ord r
                          , IsIntersectableWith (HalfLine point) (ClosedLineSegment corner)
                          , Intersection (HalfLine point) (ClosedLineSegment corner)
                            ~ Maybe (HalfLineLineSegmentIntersection (Point 2 r)
                                                                     (ClosedLineSegment corner))
                          , HasOuterBoundary triangle, Eq (VertexIx triangle)

                          , Show point, Show r, Show corner
                          , Show triangle
                          )
                       => HalfLine point -> HalfLine point -> triangle
                       -> NonEmpty (Point 2 r)
extraPoints hp hq tri | traceShow ("extraPoints",hp,hq,tri) False = undefined
extraPoints hp hq tri = noDuplicates $ q :| cornersInBetween qSide pSide tri <> [p]
    -- if the intersection point coincides with a corner then the current code includes
    -- the corner. We use the noDuplicates to get rid of those.
  where
    (q,qSide) = intersectionPoint hq
    (p,pSide) = intersectionPoint hp

    intersectionPoint  h = case getFirst $ intersectionPoint' h of
                             Nothing -> error "extraPoints: precondititon failed "
                             Just x  -> x

    intersectionPoint' h = flip (ifoldMapOf outerBoundaryEdgeSegments) tri $ \(side,_) seg ->
      case traceShowWith (h,seg,"  intersects in ",) $
        h `intersect` seg of
        Just (HalfLine_x_LineSegment_Point x) -> First $ Just (x, side)
        _                                     -> First   Nothing

    noDuplicates = fmap NonEmpty.head . NonEmpty.group1

cornersInBetween         :: ( HasVertices triangle triangle, Eq (VertexIx triangle)
                            , Point_ (Vertex triangle) 2 r
                            )
                         => VertexIx triangle -> VertexIx triangle
                         -> triangle -> [Point 2 r]
cornersInBetween s e tri = map (^._2.asPoint)
                         . takeWhile ((/= e) . fst) . dropWhile ((/= s) . fst)
                         $ cycle (itoListOf vertices tri)

-- TODO: there is a bunch of code duplication here with intersecting a rectangle in Type

----------------------------------------


-- NEMap plane (Region r vertex)


-- | Given the vertices of the lower envelope; compute the minimization diagram.
--
-- \(O(h\log h)\) assuming that the input is non-degenerate.
fromVertexForm :: ( Plane_ plane r, Ord plane, Ord r, Fractional r, Show r, Show plane
                  , HasDefiners vertex plane
                  , Ord vertex -- figure out why we need this?
                  )
               => NEMap (Point 3 r) vertex
               -> MinimizationDiagram r (Point 2 r :+ vertex) plane
fromVertexForm = MinimizationDiagram
               . Map.mapWithKey sortAroundBoundary
               . mapWithKeyMerge1 (\v vertexData ->
                    foldMap1 (\h -> Map.singleton h (Set.singleton (v,vertexData)))
                             (definersOf vertexData))

-- for each vertex v, we go through its definers defs; and for each such plane h, we
-- associate it with with the set {(v,defs)}. the foldMapWithKey part thus collects all
-- vertices (together with their definers) incident to h. i.e. it combines these sets {(v,
-- defsV)} and {(u, defsU)} etc into one big set with all vertices.

-- TODO: we should improve the running time of merge for the definers from O(k^2) to
-- O(k\log k). that should bring the overall running time to O(nlog n) even for degenerate
-- inputs.

-- | Given a plane h, and the set of vertices incident to h, compute the corresponding
-- region in the minimization diagram.
sortAroundBoundary            :: ( Plane_ plane r, Ord r, Fractional r, Ord plane
                                 , HasDefiners vertex plane
                                 )
                              => plane -> NESet (Point 3 r, vertex)
                              -> Region r (Point 2 r :+ vertex)
sortAroundBoundary h vertices = case NonEmpty.nonEmpty rest of
    Nothing     -> let (u,p,w) = singleVertex h v0 in Unbounded u (NonEmpty.singleton p) w
    Just rest'  -> let edges = NonEmpty.zip vertices' (rest' <> vertices')
                   in case NonEmpty.break (isInvalid h) edges of
                        (_,  [])         -> boundedRegion $ uncurry (:+) <$> vertices'
                        (vs, (u,v) : ws) -> let chain = fmap (uncurry (:+) . fst)
                                                      . NonEmpty.fromList
                                                      $ ws <> vs <> [(u,v)]
                                            in unboundedRegion h chain v u
  where
    vertices'@(v0 :| rest) = inCCWOrder . fmap project . Set.toList $ vertices
    boundedRegion = Bounded . fromNonEmpty
    -- The main idea is to test if the region is unbounded; i.e. containing an "edge"
    -- that is not really an edge. If so, we break open the edges there, and
    -- construct an appropriate chain from it.

-- | Given a plane h, and the single vertex v incident to the region of h, computes the
-- unbounded region for h.
--
-- In particular, returns the triple (u,p,w). h is the region to the left of u, and also the
-- left of w.
singleVertex           :: ( Plane_ plane r, Ord r, Fractional r, Ord plane
                          , HasDefiners vData plane
                          )
                       => plane -> (Point 2 r, vData)
                       -> (Vector 2 r, Point 2 r :+ vData, Vector 2 r)
singleVertex h (v,vData) = fromMaybe (error "singleVertex: absurd") $
    do (hPred,hSucc) <- findNeighbours h (definersOf vData)
       u             <- intersectionVector h hPred
       w             <- intersectionVector h hSucc
       pure (w, v :+ vData, u)

-- | Given:
--
-- - plane h bounding an unbounded region R
-- - the chain of vertices bounding R
-- - the first vertex v (with its definers)
-- - the last vertex v (with its definers)
--
-- computes the actual region R. In particular, we compute the direction that the
-- unbounded edges have.
unboundedRegion              :: forall plane r vertexData.
                                ( Plane_ plane r, Ord r, Fractional r, Ord plane
                                , HasDefiners vertexData plane
                                )
                             => plane
                             -> NonEmpty (Point 2 r :+ vertexData)
                             -> (Point 2 r, vertexData) -> (Point 2 r, vertexData)
                             -> Region r (Point 2 r :+ vertexData)
unboundedRegion h chain v@(v',_) u@(u',_)  = Unbounded wv chain wu
  where
    (v1,_,v2) = singleVertex h v
    (u1,_,u2) = singleVertex h u

    z  = u' .-. v'
    wv = case ccwCmpAroundWith z (origin :: Point 2 r) (Point $ negated v1) (Point $ negated v2) of
           LT -> v1
           EQ -> v2 -- this probably shouldn't happen
           GT -> v2
    wu = case ccwCmpAroundWith z (origin :: Point 2 r) (Point u1) (Point u2) of
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
isInvalid                          :: ( Plane_ plane r, Eq plane, Ord r, Fractional r
                                      , HasDefiners vertexData plane
                                      )
                                   => plane
                                   -> ( (Point 2 r, vertexData)
                                      , (Point 2 r, vertexData)
                                      ) -> Bool
isInvalid h ((_u,uData), (_v,vData)) =
  fromMaybe (error "isInvalid: h not found in the definers!?") $ do
      (hPredU,_) <- findNeighbours h (definersOf uData)
      (_,hSuccV) <- findNeighbours h (definersOf vData)
      pure $ hPredU /= hSuccV
    -- if (u,v) is actually a valid edge, i.e. it has h to its left, then the CCW
    -- successor w.r.t to v, and the CCW predecessor w.r.t u must be the same plane.
    -- if that is not the case the edge must be invalid.

-- | Project the vertex onto the plane.
project                     :: (Point 3 r, a) -> (Point 2 r, a)
project (Point3 x y _, loc) = (Point2 x y, loc)

-- | Given a list of vertices of a (possibly unbounded) convex polygonal region (in
-- arbitrary orientation), sort the vertices so that they are listed in CCW order.
inCCWOrder     :: (Ord r, Fractional r) => NonEmpty (Point 2 r, a) -> NonEmpty (Point 2 r, a)
inCCWOrder pts = case pts of
  ((p,_):|(q,_):_) -> let c               = p .+^ ((q .-. p) ^/ 2)
                          cmp (a,_) (b,_) = ccwCmpAround c a b <> cmpByDistanceTo c a b
                      in NonEmpty.sortBy cmp pts
  _                -> pts -- already sorted.
