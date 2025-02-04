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
import           Data.Foldable
import           Data.Foldable1
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Map.NonEmpty (NEMap)
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
import           HGeometry.Triangle
import           HGeometry.Vector
import           Prelude hiding (filter, head, last)
import           System.Random
import           Witherable

import Debug.Trace
--------------------------------------------------------------------------------

-- n_0 :: Int
-- n_0 = 2

--------------------------------------------------------------------------------

computeVertexForm        :: ( Plane_ plane r, Ord plane, Ord r, Fractional r, Foldable set
                            , RandomGen gen
                            , Show plane, Show r
                            )
                         => gen -> set plane -> VertexForm r plane
computeVertexForm gen = computeVertexForm' . shuffle gen

-- | pre: imput is already a random permutation
computeVertexForm'    :: forall plane r.
                         ( Plane_ plane r, Ord plane, Ord r, Fractional r
                         , Show plane, Show r
                         )
                      => V.Vector plane -> VertexForm r plane
computeVertexForm' hs = NEMap.unsafeFromMap $ lowerEnvelope hs
  where
    n  = length hs
    r  = sqrt . sqrt @Double . fromIntegral $ n
    r' = max 3 $ round $ r * logBase 2 r
      -- take a sample of size r*log r

    lowerEnvelope        :: Foldable set => set plane -> Map (Point 3 r) (Definers plane)
    lowerEnvelope planes | traceShow ("LE",toList planes) False = undefined
    lowerEnvelope planes
        | null planes = mempty
        | otherwise   = NEMap.mapMaybe hasNoConflict verticesRNet
                        <>
                        foldMap lowerEnvelopeIn triangulatedEnv
      where
        (rNet,remaining) = takeSample r' planes

        verticesRNet    :: NEMap (Point 3 r) (Definers plane, Set plane)
        verticesRNet    = withConflictLists remaining $ BruteForce.computeVertexForm rNet

        triangulatedEnv :: NonEmpty (Triangular r (Point 2 r :+ Set plane))
        triangulatedEnv = toTriangles . mapVertices (&extra %~ snd)
                        . fromVertexForm $ verticesRNet

    lowerEnvelopeIn     :: (Foldable set, Monoid (set plane))
                        => Triangular r (Point 2 r :+ set plane)
                        -> Map (Point 3 r) (Definers plane)
    lowerEnvelopeIn tri | traceShow ("LEI",toList $ conflictListOf tri) False = undefined
    lowerEnvelopeIn tri = Map.filterWithKey (inRegion tri)
                        $ lowerEnvelope (conflictListOf tri)

-- | Given a size r; take a sample of the planes from the given size (essentially by just
-- taking the first r planes.)
takeSample   :: (Foldable set, Ord plane) => Int -> set plane -> ([plane],Set plane)
takeSample r = fmap Set.fromList . splitAt r . toList

-- | Test whether the given point lies inside the triangular region.
inRegion         :: (Ord r, Num r, Point_ vertex 2 r)
                 => Triangular r vertex -> Point 3 r -> a -> Bool
inRegion tri v _ = all (projectPoint v `intersects`) (halfspaces tri)


-- | The triangular region is the intersection of halfspaces; compute this set of halfspaces.
halfspaces :: (Point_ vertex 2 r, Num r, Ord r)
           => Triangular r vertex -> NonEmpty (HalfSpaceF (LinePV 2 r))
halfspaces tr = case view asPoint <$> tr of
  Triangular triangle  -> toNonEmpty $ intersectingHalfPlanes triangle
  UnboundedOne u p v   -> leftHalfPlane <$> (LinePV p u) :| [ LinePV p v ]
  UnboundedTwo u p q v -> leftHalfPlane <$> (LinePV p u) :| [ LinePV p (q .-. p), LinePV q v ]




hasNoConflict                     :: Foldable set => (definers, set plane) -> Maybe definers
hasNoConflict (defs,conflictList)
  | null conflictList              = Just defs
  | otherwise                      = Nothing

-- | Get the conflictList of a triangle
conflictListOf :: Monoid conflictList => Triangular r (vertex :+ conflictList) -> conflictList
conflictListOf = foldMap (^.extra)

--FIXME: This is wrong; for unbounded regions; we are missing planes.
-- So maybe compute everything in a bounding box/triagnle instead after all.

-- | Computes conflict list
withConflictLists        :: (Plane_ plane r, Ord r, Num r)
                         => Set plane
                         -> VertexForm r plane
                         -> NEMap (Point 3 r) (Definers plane, Set plane)
withConflictLists planes = NEMap.mapWithKey (\v defs -> (defs, Set.filter (below v) planes))
  where
    below v h = verticalSideTest v h == LT -- TODO: not sure if this should be LT or 'not GT'
-- TODO: dummy implementation for now


data Triangular r vertex = Triangular   (Triangle vertex)
                         | UnboundedOne (Vector 2 r) vertex (Vector 2 r)
                         | UnboundedTwo (Vector 2 r) vertex vertex (Vector 2 r)
                         deriving (Show,Eq,Functor,Foldable)


-- | Triangulate the minimization diagram.
toTriangles :: MinimizationDiagram r vertex plane -> NonEmpty (Triangular r vertex)
toTriangles = foldMap1 toTriangles' . asMap
  where
    toTriangles' = \case
      Bounded vertices       -> case toNonEmpty vertices of
                                  u :| (v : w : vs) -> triangulate u v (w:|vs)
                                  _                 -> error "bounded reg with < 3 vertices.."
      Unbounded u vertices v -> case vertices of
                                  p :| []       -> UnboundedOne u p v :| []
                                  p :| (q:rest) -> let z = last $ q:|rest in
                                                   UnboundedTwo u p z v :| triangulate' p q rest

triangulate        :: vertex -> vertex -> NonEmpty vertex -> NonEmpty (Triangular r vertex)
triangulate u v vs = Triangular <$> NonEmpty.zipWith (Triangle u) (v NonEmpty.<| vs) vs

triangulate'     :: vertex -> vertex -> [vertex] -> [Triangular r vertex]
triangulate' u v = maybe [] (toList . triangulate u v) . NonEmpty.nonEmpty


----------------------------------------
