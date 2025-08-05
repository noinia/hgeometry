--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Plane.LowerEnvelope.Connected.Randomized
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- expected O(n polylog n) divide and conquer implementation
--
--------------------------------------------------------------------------------
module HGeometry.Plane.LowerEnvelope.Connected.Randomized
  ( computeVertexForm
  ) where

import           Control.Lens hiding (below)
import           Data.Bifunctor
import           Data.Foldable
import           Data.Foldable1
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Map.NonEmpty (NEMap, pattern IsEmpty, pattern IsNonEmpty)
import qualified Data.Map.NonEmpty as NEMap
import           Data.Maybe (mapMaybe)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Set.NonEmpty (NESet)
import qualified Data.Set.NonEmpty as NESet
import qualified Data.Vector.NonEmpty as V
import           HGeometry.Ext
import           HGeometry.Foldable.Util
import           HGeometry.HyperPlane.Class
import           HGeometry.HyperPlane.NonVertical
import           HGeometry.Intersection
import           HGeometry.Permutation.Shuffle
import           HGeometry.Plane.LowerEnvelope.Clipped.Type
import qualified HGeometry.Plane.LowerEnvelope.Connected.BruteForce as BruteForce
import           HGeometry.Plane.LowerEnvelope.Connected.Region
import           HGeometry.Plane.LowerEnvelope.Connected.Regions
import           HGeometry.Plane.LowerEnvelope.Connected.VertexForm (definersOf)
import           HGeometry.Point
import           HGeometry.Point.Either
import           HGeometry.Polygon
import           HGeometry.Polygon.Simple.PossiblyDegenerate
import           HGeometry.Triangle
import           Prelude hiding (filter, head, last)
import           System.Random


import           Debug.Trace
--------------------------------------------------------------------------------

-- n_0 :: Int
-- n_0 = 2

--------------------------------------------------------------------------------

computeVertexForm        :: ( Plane_ plane r, Ord plane, Ord r, Fractional r, Foldable1 set
                            , RandomGen gen
                            , Show plane, Show r
                            )
                         => gen -> set plane -> VertexForm Map r plane
computeVertexForm gen = computeVertexFormIn tri . V.unsafeFromVector . shuffle gen
    -- since the set is non-empty, the V.unsafeFromVector is actually also safe
  where
    tri = Triangle (Point2 (-100_000) (-100_000))
                   (Point2 (-100_000) (200_000))
                   (Point2 (200_000)  (-100_000))
    -- TODO: compute abounding box/triangle instead

-- | pre:
--
-- - input is already a random permutation
computeVertexFormIn         :: forall plane point r.
                              ( Plane_ plane r, Ord plane, Ord r, Fractional r
                              , Show plane, Show r
                              , Point_ point 2 r
                              )
                            => Triangle point
                            -> V.NonEmptyVector plane
                            -> VertexForm Map r plane
computeVertexFormIn tri0 hs = lowerEnvelopeIn (view asPoint <$> tri0) hs
  where
    n  = length hs
    r  = sqrt . sqrt @Double . fromIntegral $ n -- r = n^{1/4}
    r' = max 5 $ round $ r * logBase 2 r
      -- take a sample of size r*log r
      --
      -- the max 5 is due to the following: consider some triangle defined by the plane
      -- h_0, and let h_1, h_2, h_3, be the planes that intersect h_0 on the edges of the
      -- triangle. If this triangle has a conflict list, i.e. there is a plane h that
      -- intersects below a vertex, then we need to include at least h_0,..,h_3,h. Hence,
      -- we need at least 5 planes.

      -- TODO: make sure that if the lower envelope has a vertex, we select planes
      -- that generate a plane.

    lowerEnvelopeIn   :: Foldable1 set
                      => Triangle (Point 2 r)
                      -> set plane
                      -> Map (Point 3 r) (Definers plane)
    lowerEnvelopeIn tri planes | traceShow ("LE",tri, toList planes) False = undefined

    lowerEnvelopeIn tri planes = let (rNet,remaining) = traceShowWith ("rnet",) $ takeSample r' planes in
      case withConflictLists remaining (BruteForce.computeVertexForm rNet) of
        IsEmpty                                                                    -> mempty
        IsNonEmpty (verticesRNet :: NEMap (Point 3 r) (Definers plane, Set plane)) ->
          NEMap.mapMaybeWithKey (asVertexIn tri) verticesRNet
            `merge` foldMap lowerEnvelopeIn' triangulatedEnv
          where
            -- Construct the lower envelope inside the triangle.
            -- also compute the conflicts of the corners
            env :: NEMap plane (ClippedMDCell'' r (MDVertex r plane (_, Set plane))
                                                  (Point 2 r :+ Set plane)
                               )
            env = traceShowWith ("Env",) $
                  withExtraConflictLists remaining
                . fromVertexFormIn tri rNet $ verticesRNet

            -- | Triangulate the lower envelope, and collect the conflict list of each
            -- prism. We will already throw away any prisms with an
            triangulatedEnv :: [Triangle (Point 2 r) :+ NESet plane]
            triangulatedEnv = NEMap.foldMapWithKey triangulate env

            -- merge the resulting Maps; making sure to actually combine the definers
            merge = Map.unionWithKey mergeDefiners


    lowerEnvelopeIn'     :: Triangle (Point 2 r) :+ NESet plane
                         -> Map (Point 3 r) (Definers plane)
    lowerEnvelopeIn' (tri :+ conflictList) = lowerEnvelopeIn tri conflictList

    -- tri = case conflictListOf tri of
    --   NESet.IsNonEmpty conflictList ->  (view core <$> tri) conflictList
    --   _                             -> mempty


-- dropVertexData :: ClippedBoundedRegion r (MDVertex r plane (a,b)) x
--                -> ClippedBoundedRegion r (MDVertex r plane b) x
-- dropVertexData = fmap $ first (fmap snd)

-- | Given a size r > 0; take a sample of the planes from the given size (essentially by just
-- taking the first r planes.)
--
-- pre: r > 0
takeSample   :: (Foldable1 set, Ord plane) => Int -> set plane -> (NonEmpty plane,Set plane)
takeSample r = bimap NonEmpty.fromList Set.fromList . splitAt r . toList


-- | Report whether this is really a vertex of the global lower envelope in the region.
asVertexIn :: (Ord r, Num r, Point_ vertex 2 r, Foldable set
              , Show definers, Show (set plane), Show r, Show vertex
              )
           => Triangle vertex -> Point 3 r -> (definers, set plane) -> Maybe definers
asVertexIn tri (Point3 x y _) (defs,conflictList)
  | traceShow ("asVertexIn ",tri,x,y, conflictList, null conflictList && (Point2 x y) `intersects` tri) False = undefined
  | null conflictList && (Point2 x y) `intersects'` tri = Just defs
  | otherwise                                           = Nothing
  where
    p `intersects'` tri = p `intersects` tri && notElemOf (vertices.asPoint) p tri


-- -- | Test whether the given point lies inside the triangular region.
-- inRegion         :: (Ord r, Num r, Point_ vertex 2 r)
--                  => Triangle vertex -> Point 3 r -> a -> Bool
-- inRegion tri v _ = projectPoint v `intersects` tri

--   -- all () (halfspaces tri)


-- -- | The triangular region is the intersection of halfspaces; compute this set of halfspaces.
-- halfspaces :: (Point_ vertex 2 r, Num r, Ord r)
--            => Triangular r vertex -> NonEmpty (HalfSpaceF (LinePV 2 r))
-- halfspaces tr = case view asPoint <$> tr of
--   Triangular triangle  -> toNonEmpty $ intersectingHalfPlanes triangle
--   UnboundedOne u p v   -> leftHalfPlane <$> (LinePV p u) :| [ LinePV p v ]
--   UnboundedTwo u p q v -> leftHalfPlane <$> (LinePV p u) :| [ LinePV p (q .-. p), LinePV q v ]

hasNoConflict                     :: Foldable set => (definers, set plane) -> Maybe definers
hasNoConflict (defs,conflictList)
  | null conflictList              = Just defs
  | otherwise                      = Nothing

-- | Get the conflictList of a triangle
conflictListOf :: Monoid conflictList => Triangle (vertex :+ conflictList) -> conflictList
conflictListOf = foldMap (^.extra)

-- | Computes conflict list
withConflictLists        :: ( Plane_ plane r, Ord r, Num r, Ord plane
                            , FunctorWithIndex (Point 3 r) (map (Point 3 r))
                            , Show r, Show plane
                            )
                         => Set plane
                         -> VertexForm map r plane
                         -> map (Point 3 r) (Definers plane, Set plane)
withConflictLists planes = imap (\v defs -> (defs, Set.filter (below v) planes))
  where
    below v h =
      traceShowWith ("below ",v,h,) $
      verticalSideTest v h == LT
      -- a plane conflicts with a vertex v if it passes strictly below the point


-- | Compute the conflit lists for the extra vertices we added.
withExtraConflictLists        :: (Plane_ plane r, Ord r, Num r--, Point_ corner 2 r
                                 )
                              => Set plane
                              -> NEMap plane (ClippedMDCell r plane (a, Set plane))
                              -> NEMap plane (ClippedMDCell'' r (MDVertex r plane (a, Set plane))
                                                                (Point 2 r :+ Set plane))
withExtraConflictLists planes = NEMap.mapWithKey $ \h -> fmap (withPolygonVertex h)
  where
    withPolygonVertex h v = v :+ Set.filter (below (evalAt v h) v) planes
    below z (Point2_ x y) h = verticalSideTest (Point3 x y z) h  == LT


  -- TODO
    -- note that we use the /= GT, so that we include the plane that define
    -- the lower envelope on this place.
  -- TODO: do we really want this?


-- withConflictLists
-- withConflictLists planes = undefined
-- {-

                         -- NEMap (Point 3 r) (Definers plane, Set plane)
-- withConflictLists planes = NEMap.mapWithKey (\v defs -> (defs, Set.filter (below v) planes))
--   where
--     below v h = verticalSideTest v h == LT -- TODO: not sure if this should be LT or 'not GT'
-- TODO: dummy implementation for now
--}

-- data Triangular r vertex = Triangular   (Triangle vertex)
--                          | UnboundedOne (Vector 2 r) vertex (Vector 2 r)
--                          | UnboundedTwo (Vector 2 r) vertex vertex (Vector 2 r)
--                          deriving (Show,Eq,Functor,Foldable)


-- -- | Triangulate the minimization diagram.
-- toTriangles :: MinimizationDiagram r vertex plane -> NonEmpty (Triangular r vertex)
-- toTriangles = foldMap1 toTriangles' . asMap
--   where
--     toTriangles' = \case
--       Bounded vertices       -> case toNonEmpty vertices of
--                                   u :| (v : w : vs) -> triangulate u v (w:|vs)
--                                   _                 -> error "bounded reg with < 3 vertices.."
--       Unbounded u vertices v -> case vertices of
--                                   p :| []       -> UnboundedOne u p v :| []
--                                   p :| (q:rest) -> let z = last $ q:|rest in
--                                                    UnboundedTwo u p z v :| triangulate' p q rest



                 -- ClippedBoundedRegion r (MDVertex r plane (a, conflictList))
                 --                           (Point 2 r :+ conflictList)
triangulate                        :: (Num r, Ord plane)
                                   => plane
                                   -> ClippedMDCell'' r (MDVertex r plane (a, Set plane))
                                                        (Point 2 r :+ Set  plane)
                                   -> [Triangle (Point 2 r) :+ NESet plane]
triangulate h (ClippedMDCell cell) = case cell of
    ActualPolygon poly -> triangulate' h poly
    DegenerateVertex _ -> []
    DegenerateEdge   _ -> []
  -- TODO: technically, there could still be vertices on the degenerate edge.
  -- make sure we hanlde those somewhere else

triangulate'        :: (Num r, Ord plane)
                    => plane
                    -> ClippedBoundedRegion r (MDVertex r plane (a, Set plane))
                                              (Point 2 r :+ Set plane)
                    -> [Triangle (Point 2 r) :+ NESet plane]
triangulate' h poly = mapMaybe' withConflictList $ case toNonEmptyOf vertices poly of
    u :| (v : vs) -> NonEmpty.zipWith (Triangle u) (v :| vs) (NonEmpty.fromList vs)
    _             -> error "absurd. trianglulate; impossible"
  where
    withConflictList tri = case foldMap conflictListOf tri of
      NESet.IsEmpty                 -> Nothing
      NESet.IsNonEmpty conflictList -> Just $
        let conflictList' = foldr NESet.insert conflictList (foldMap edgeDefiners $ edgesOf tri)
        in ((^.asPoint) <$> tri) :+ conflictList'

    edgesOf (Triangle a b c) = [(a,b),(b,c),(c,a)]

    -- computes the edge definers of the edge u,v. Note that this will typically consist
    -- of two planes; h, and the other plane h_(u,v) on the other side of the edge.
    edgeDefiners (u,v) = definersOf' u `Set.intersection` definersOf' v
    definersOf' = \case
      Original v -> foldMap Set.singleton $ definersOf v
      Extra    _ -> Set.singleton h
    -- TODO: hmm, not quite sure if the h is right

    -- get the conflictList as a vertex
    conflictListOf = \case
      Original v -> v^.vertexData._2
      Extra    p -> p^.extra

    mapMaybe' f = mapMaybe f . toList

    -- neSet <>> set' = foldr NESet.insert neSet set'


----------------------------------------
