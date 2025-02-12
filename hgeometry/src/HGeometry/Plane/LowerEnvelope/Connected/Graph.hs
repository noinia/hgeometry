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
  , toTriangulatedPlaneGraph'
  , toPlaneGraph'
  ) where

import           Data.Foldable1
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Map.NonEmpty (NEMap)
import qualified Data.Map.NonEmpty as NEMap
import           Data.Semigroup (First(..))
import           HGeometry.HyperPlane.Class
import           HGeometry.HyperPlane.NonVertical
import           HGeometry.Plane.LowerEnvelope.Connected.MonoidalMap
import           HGeometry.Plane.LowerEnvelope.Connected.Type
import           HGeometry.PlaneGraph.Connected.Type (E(..))
import           HGeometry.Point
import           HGeometry.Vector
import           Hiraffe.AdjacencyListRep.Map
--------------------------------------------------------------------------------

-- | A Plane graph storing vertices of type v that are identified by keys of type k, and
-- some ordered sequence of edges (which are ordered using e).
type PlaneGraph' k v e = GGraph (NEMap e) k v e

-- | A Plane graph storing vertices of type v that are identified by keys of type k, and
-- some ordered sequence of edges (which are ordered using e).
type PlaneGraphMap k v e = NEMap k (NEMap e k, v)


-- | Produce a triangulated plane graph on the bounded vertices.  every vertex is
-- represented by its point, it stores a list of its outgoing edges, and some data.
toTriangulatedPlaneGraph' :: (Plane_ plane r, Num r, Ord r)
                          => MinimizationDiagram r (Point 2 r) plane
                          -> PlaneGraph' (Point 2 r) (First r) (E r)
toTriangulatedPlaneGraph' = toPlaneGraph'' . toTriangulatedPlaneGraphMap

-- | Produce a plane graph on the bounded vertices.  every vertex is represented by its
-- point, it stores a list of its outgoing edges, and some data.
toPlaneGraph' :: (Plane_ plane r, Num r, Ord r)
             => MinimizationDiagram r  (Point 2 r) plane -> PlaneGraph' (Point 2 r) (First r) (E r)
toPlaneGraph' = toPlaneGraph'' . toPlaneGraphMap

-- | Helper to produce the graphs
toPlaneGraph'' :: (Ord r)
               => PlaneGraphMap (Point 2 r) (First r) (E r)
               -> PlaneGraph' (Point 2 r) (First r) (E r)
toPlaneGraph'' = Graph
               . fmap (\(neighOrder, x) -> VertexData x (mkNeighMap neighOrder) neighOrder)
  where
    mkNeighMap = NEMap.foldMapWithKey (\e i -> Map.singleton i e)

--------------------------------------------------------------------------------

-- | Produce a triangulated plane graph on the bounded vertices.  every vertex is
-- represented by its point, it stores a list of its outgoing edges, and some data.
toTriangulatedPlaneGraphMap :: (Plane_ plane r, Num r, Ord r)
                            => MinimizationDiagram r  (Point 2 r) plane
                            -> PlaneGraphMap (Point 2 r) (First r) (E r)
toTriangulatedPlaneGraphMap = mapWithKeyMerge1 toTriangulatedGr . asMap


-- | Helper function to construct a triangulated plane graph
toTriangulatedGr   :: (Plane_ plane r, Num r, Ord r)
                   => plane -> Region r (Point 2 r)
                   -> PlaneGraphMap (Point 2 r) (First r) (E r)
toTriangulatedGr h = NEMap.unsafeFromMap
                   . Map.mapWithKey (\v adjs -> (adjs, First $ evalAt v h)) . \case
  Bounded vertices       -> case toNonEmpty vertices of
    (v0:|v1:vs) -> triangulate v0 v1 vs
    _           -> error "triangulate: absurd, <2 vertices"
  Unbounded _ vertices _ -> case vertices of
    _  :| []     -> Map.empty
    u  :| [v]    -> Map.fromList [ (u, (uncurry NEMap.singleton $ edge u v))
                                 , (v, (uncurry NEMap.singleton $ edge v u))
                                 ]
    v0 :|(v1:vs) -> triangulate v0 v1 vs
  where
    triangulate v0 v1 vs = Map.unionsWith (<>) $ zipWith (triangle v0) (v1:vs) vs

    triangle u v w = Map.fromList [ (u, mkNEMap [ edge u v, edge u w])
                                  , (v, mkNEMap [ edge v u, edge v w])
                                  , (w, mkNEMap [ edge w u, edge w v])
                                  ]
    edge u v = ((E $ v .-. u), v)

--------------------------------------------------------------------------------

-- | Produce a plane graph on the bounded vertices.  every vertex is represented by its
-- point, it stores a list of its outgoing edges (to other bounded vertices), and some
-- data.
toPlaneGraphMap :: (Plane_ plane r, Num r, Ord r)
                => MinimizationDiagram r  (Point 2 r) plane
                -> PlaneGraphMap (Point 2 r) (First r) (E r)
toPlaneGraphMap = mapWithKeyMerge1 toGr . asMap


-- | Helper function to construct a plane graph
toGr   :: (Plane_ plane r, Num r, Ord r)
       => plane -> Region r (Point 2 r)
       -> PlaneGraphMap (Point 2 r) (First r) (E r)
toGr h = NEMap.unsafeFromMap
       . Map.mapWithKey (\v adjs -> (adjs, First $ evalAt v h)) . \case
  Bounded vertices       -> case toNonEmpty vertices of
    verts@(_:|v1:vs) -> Map.unionsWith (<>) $ NonEmpty.zipWith mkEdge verts (v1:|vs)
    _                -> error "toGR: absurd, <2 vertices"
  Unbounded _ vertices _ -> case vertices of
    _  :| []  -> Map.empty
    u  :| vs  -> Map.unionsWith (<>) $ zipWith mkEdge (u:vs) vs
  where
    mkEdge u v = Map.fromList [ (u, (uncurry NEMap.singleton $ edge u v))
                              , (v, (uncurry NEMap.singleton $ edge v u))
                              ]
    edge u v = ((E $ v .-. u), v)

--------------------------------------------------------------------------------

-- | Helper to construct a NEMap from an explicit list of key value pairs
mkNEMap :: Ord k => [(k,v)] -> NEMap k v
mkNEMap = NEMap.fromList . NonEmpty.fromList
