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
module HGeometry.Plane.LowerEnvelope.Connected.Graph
  ( PlaneGraph'
  , E(..)
  , toPlaneGraph'
  ) where

import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Map.NonEmpty as NEMap
import           Data.Semigroup (First(..))
import           HGeometry.HyperPlane.Class
import           HGeometry.HyperPlane.NonVertical
import           HGeometry.Plane.LowerEnvelope.Connected.MonoidalMap
import           HGeometry.Plane.LowerEnvelope.Connected.Type
import           HGeometry.PlaneGraph.Type (E(..))
import           HGeometry.Point
import           HGeometry.Vector
import           Hiraffe.AdjacencyListRep.Map
--------------------------------------------------------------------------------

-- | A Plane graph storing vertices of type v that are identified by keys of type k, and
-- some ordered sequence of edges (which are ordered using e).
type PlaneGraph' k v e = GGraph (Map e) k v e



-- | A Plane graph storing vertices of type v that are identified by keys of type k, and
-- some ordered sequence of edges (which are ordered using e).
type PlaneGraphMap k v e = Map k (Map e k, v)


-- | Produce a triangulated plane graph on the bounded vertices.  every vertex is
-- represented by its point, it stores a list of its outgoing edges, and some data.
toPlaneGraph' :: (Plane_ plane r, Num r, Ord r)
             => MinimizationDiagram r plane -> PlaneGraph' (Point 2 r) (First r) (E r)
toPlaneGraph' = Graph . NEMap.unsafeFromMap
             . fmap (\(neighOrder, x) -> VertexData x (mkNeighMap neighOrder) neighOrder)
             . toPlaneGraphMap
  where
    mkNeighMap = Map.foldMapWithKey (\e i -> Map.singleton i e)


-- | Produce a triangulated plane graph on the bounded vertices.  every vertex is
-- represented by its point, it stores a list of its outgoing edges, and some data.
toPlaneGraphMap :: (Plane_ plane r, Num r, Ord r)
                => MinimizationDiagram r plane -> PlaneGraphMap (Point 2 r) (First r) (E r)
toPlaneGraphMap = mapWithKeyMerge toTriangulatedGr . asMap

-- | Helper function to construct a triangulated plane graph
toTriangulatedGr   :: (Plane_ plane r, Num r, Ord r)
                   => plane -> Region r (Point 2 r)
                   -> PlaneGraphMap (Point 2 r) (First r) (E r)
toTriangulatedGr h = Map.mapWithKey (\v adjs -> (adjs, First $ evalAt v h)) . \case
  Bounded vertices       -> case vertices of
    (v0:v1:vs) -> triangulate v0 v1 vs
    _          -> error "triangulate: absurd, <2 vertices"
  Unbounded _ vertices _ -> case vertices of
    _  :| []     -> Map.empty
    u  :| [v]    -> Map.fromList [ (u, (uncurry Map.singleton $ edge u v))
                                 , (v, (uncurry Map.singleton $ edge v u))
                                 ]
    v0 :|(v1:vs) -> triangulate v0 v1 vs
  where
    triangulate v0 v1 vs = Map.unionsWith (<>) $ zipWith (triangle v0) (v1:vs) vs

    triangle u v w = Map.fromList [ (u, Map.fromList [ edge u v, edge u w])
                                  , (v, Map.fromList [ edge v u, edge v w])
                                  , (w, Map.fromList [ edge w u, edge w v])
                                  ]
    edge u v = ((E $ v .-. u), v)
