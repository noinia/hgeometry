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

import           Control.Lens
import           Data.Bifunctor
import           Data.Foldable
import           Data.Foldable1
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Map.NonEmpty (NEMap, pattern IsEmpty, pattern IsNonEmpty)
import qualified Data.Map.NonEmpty as NEMap
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Vector as V
import           HGeometry.Ext
import           HGeometry.HalfSpace
import           HGeometry.HyperPlane.Class
import           HGeometry.HyperPlane.NonVertical
import           HGeometry.Intersection
import           HGeometry.Line.PointAndVector
import           HGeometry.Permutation.Shuffle
import qualified HGeometry.Plane.LowerEnvelope.Connected.BruteForce as BruteForce
import           HGeometry.Plane.LowerEnvelope.Connected.MonoidalMap
import           HGeometry.Plane.LowerEnvelope.Connected.Regions
import           HGeometry.Plane.LowerEnvelope.Connected.Type
import           HGeometry.Point
import           HGeometry.Point.Either
import           HGeometry.Polygon
import           HGeometry.Triangle
import           HGeometry.Vector
import           Prelude hiding (filter, head, last)
import           System.Random
import           Witherable


import           Debug.Trace
--------------------------------------------------------------------------------

-- n_0 :: Int
-- n_0 = 2

--------------------------------------------------------------------------------

computeVertexForm        :: ( Plane_ plane r, Ord plane, Ord r, Fractional r, Foldable set
                            , RandomGen gen
                            , Show plane, Show r
                            )
                         => gen -> set plane -> VertexForm Map r plane
computeVertexForm gen = computeVertexFormIn tri . shuffle gen
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
                            -> V.Vector plane
                            -> VertexForm Map r plane
computeVertexFormIn tri0 hs = lowerEnvelopeIn (view asPoint <$> tri0) hs
  where
    n  = length hs
    r  = sqrt . sqrt @Double . fromIntegral $ n
    r' = max 3 $ round $ r * logBase 2 r
      -- take a sample of size r*log r

    lowerEnvelopeIn   :: (Foldable set)
                      => Triangle (Point 2 r)
                      -> set plane
                      -> Map (Point 3 r) (Definers plane)
    lowerEnvelopeIn tri planes | traceShow ("LE",toList planes) False = undefined
    lowerEnvelopeIn tri planes =
      let
          (rNet,remaining) = takeSample r' planes

          verticesRNet    :: Map (Point 3 r) (Definers plane, Set plane)
          verticesRNet    = withConflictLists remaining $ BruteForce.computeVertexForm rNet
      in case verticesRNet of
           IsEmpty                  -> mempty
           IsNonEmpty verticesRNet' -> NEMap.mapMaybeWithKey (asVertexIn tri) verticesRNet'
                                       <>
                                       foldMap lowerEnvelopeIn' triangulatedEnv
             where
               env :: NEMap plane (BoundedRegion r  (Point 2 r :+ (Definers plane, Set plane))
                                                    (Point 2 r :+ Set plane))
               env = withExtraConflictLists remaining
                   . fromVertexFormIn tri $ verticesRNet'
               -- also compute the conflicts of the extra vertices

               triangulatedEnv :: NonEmpty (Triangle (Point 2 r :+ Set plane))
               triangulatedEnv = foldMap1 triangulate env


    lowerEnvelopeIn'     :: (Foldable set, Monoid (set plane))
                         => Triangle (Point 2 r :+ set plane)
                         -> Map (Point 3 r) (Definers plane)
    lowerEnvelopeIn' tri = lowerEnvelopeIn (view core <$> tri) (conflictListOf tri)



-- | Given a size r; take a sample of the planes from the given size (essentially by just
-- taking the first r planes.)
takeSample   :: (Foldable set, Ord plane) => Int -> set plane -> ([plane],Set plane)
takeSample r = fmap Set.fromList . splitAt r . toList


-- | Report whether this is really a vertex of the global lower envelope in the region.
asVertexIn :: (Ord r, Num r, Point_ vertex 2 r, Foldable set)
           => Triangle vertex -> Point 3 r -> (definers, set plane) -> Maybe definers
asVertexIn tri (Point3 x y _) (defs,conflictList)
  | null conflictList && (Point2 x y) `intersects` tri = Just defs
  | otherwise                                          = Nothing


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

--FIXME: This is wrong; for unbounded regions; we are missing planes.
-- So maybe compute everything in a bounding box/triagnle instead after all.

-- | Computes conflict list
withConflictLists        :: ( Plane_ plane r, Ord r, Num r
                            , FunctorWithIndex (Point 3 r) (map (Point 3 r))
                            )
                         => Set plane
                         -> VertexForm map r plane
                         -> map (Point 3 r) (Definers plane, Set plane)
withConflictLists planes = imap (\v defs -> (defs, Set.filter (below v) planes))
  where
    below v h = verticalSideTest v h == LT -- TODO: not sure if this should be LT or 'not GT'

-- | Compute the conflit lists for the extra vertices we added.
withExtraConflictLists        :: (Plane_ plane r, Ord r, Num r, Point_ corner 2 r
                                 )
                              => Set plane
                              -> NEMap plane (BoundedRegion r vertex corner)
                              -> NEMap plane (BoundedRegion r vertex (corner :+ Set plane))
withExtraConflictLists planes = NEMap.mapWithKey (\h -> fmap (second $ withPolygonVertex h))
  where
    withPolygonVertex h v = v :+ Set.filter (below (evalAt v h) v) planes
    below z (Point2_ x y) h = verticalSideTest (Point3 x y z) h  == LT
  -- TODO: make this fast



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

triangulate      :: BoundedRegion r (vertex :+ (a, conflictList)) (vertex :+ conflictList)
                 -> NonEmpty (Triangle (vertex :+ conflictList))
triangulate poly = case flatten <$> toNonEmptyOf vertices poly of
    u :| (v : vs) -> NonEmpty.zipWith (Triangle u) (v :| vs) (NonEmpty.fromList vs)
    _             -> error "absurd. trianglulate; impossible"
  where
    flatten = \case
      Original (p :+ (_,cl)) -> p :+ cl
      Extra    p             -> p


----------------------------------------
