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
  ( PlaneGraph
  , E(..)
  , toPlaneGraph
  ) where

import           Control.Lens
import qualified Data.Foldable as F
import qualified Data.List as List
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe, listToMaybe)
import           Data.Semigroup (First(..))
import           Data.Set (Set)
import qualified Data.Set as Set
import           HGeometry.Combinatorial.Util
import           HGeometry.HyperPlane.Class
import           HGeometry.HyperPlane.NonVertical
import           HGeometry.Intersection
import           HGeometry.Line
import           HGeometry.Line.General
import           HGeometry.Plane.LowerEnvelope.Connected.MonoidalMap
import           HGeometry.Plane.LowerEnvelope.Connected.Regions
import           HGeometry.Point
import           HGeometry.Properties
import           HGeometry.Vector

--------------------------------------------------------------------------------

-- | A Plane graph storing vertices of type v that are identified by keys of type k, and
-- some ordered sequence of edges (which are ordered using e).
type PlaneGraph k v e = Map k (Map e k, v)



newtype E r = E (Vector 2 r)
  deriving newtype (Show)

instance (Ord r, Num r) => Eq (E r) where
  a == b = a `compare` b == EQ
instance (Ord r, Num r) => Ord (E r) where
  (E v) `compare` (E u) = ccwCmpAroundWith (Vector2 0 1) origin (Point v) (Point u)

-- | Produce a triangulated plane graph on the bounded vertices.  every vertex is
-- represented by its point, it stores a list of its outgoing edges, and some data.
toPlaneGraph :: (Plane_ plane r, Num r, Ord r)
             => MinimizationDiagram r plane -> PlaneGraph (Point 2 r) (First r) (E r)
toPlaneGraph = mapWithKeyMerge toTriangulatedGr



toTriangulatedGr   :: (Plane_ plane r, Num r, Ord r)
                   => plane -> Region r (Point 2 r)
                   -> PlaneGraph (Point 2 r) (First r) (E r)
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
