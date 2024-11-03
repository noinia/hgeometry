--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.PlaneGraph.Instances
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Arbitrary instance for a plane graph
--
--------------------------------------------------------------------------------
module HGeometry.PlaneGraph.Instances
  ( arbitraryPlaneGraph
  ) where

import Control.Lens
import Data.Coerce
import Data.Foldable1
import qualified Data.List.NonEmpty as NonEmpty
import Data.List.NonEmpty(NonEmpty(..))
import qualified Data.Map as Map
import qualified Data.Map.NonEmpty as NEMap
import Data.Ord(comparing)
import Data.Proxy
import qualified Data.Set as Set
import Data.Set(Set)
import Data.Tree
import HGeometry.Instances ()
import HGeometry.Plane.LowerEnvelope.Connected.Graph
import HGeometry.PlaneGraph
import HGeometry.Point
import HGeometry.VoronoiDiagram
import Hiraffe.AdjacencyListRep.Map
import Hiraffe.BFS.Pure
import Prelude hiding (filter)
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Witherable

--------------------------------------------------------------------------------

data QuickCheckWorld

instance ( Arbitrary r
         , Ord r
         ) => Arbitrary (PlaneGraph QuickCheckWorld (Point 2 r) () ()) where
  arbitrary = arbitraryPlaneGraph Proxy

-- general strategy:
-- 1) generate a bunch of random points uniformly at random.
-- 2) Construct the Voronoi diagram
-- 3) turn it into a triangulated graph (on its bounded vertices)
-- 4) sample a subset of its edges
-- 5) return the largest connected component


 -- | Returns the largest connected component in the graph; i.e. it shrinks
-- the graph to contain only the vertices/edges in this connected component.
largestComponent    :: (Ord i, Witherable f)
                    => GGraph f i v (Bool, e) -> GGraph f i v e
largestComponent gr = witherGraphTo tr gr
  where
    tr = maximumBy (comparing length) $ bff gr

arbitraryPlaneGraph       :: forall proxy s r.
                             (Ord r)
                          => proxy s -> Gen (PlaneGraph s (Point 2 r) () ())
arbitraryPlaneGraph proxy = do
    (pts :: NonEmpty (Point 2 r)) <- arbitrary
    case voronoiDiagram pts of
      AllColinear _  -> arbitraryPlaneGraph proxy -- retry
      ConnectedVD vd -> toPlaneGraph proxy . largestComponent
                        <$> markWitherableEdges (toPlaneGraph' . asMD $ vd)

-- asMD :: VoronoiDiagram
asMD = undefined


-- | select a random subset of edges. I.e. it marks the edges we want to retain.
markWitherableEdges    :: GGraph f i v e -> Gen (GGraph f i v (Bool,e))
markWitherableEdges gr = gr&edges %%~ \x -> (,x) <$> arbitrary

-- | Retain only the selected subset of the vertices, and the edges marked
witherGraphTo               :: ( Foldable1 g, Witherable f, Ord i
                               ) => g i -> GGraph f i v (Bool, e) -> GGraph f i v e
witherGraphTo vs (Graph gr) = Graph $ fmap removeEdges m
  where
    -- retain only the vertices from vs
    m = foldMap1 (\u -> NEMap.singleton u (gr NEMap.! u)) vs
    -- remove edges to other components
    removeEdges (VertexData x ns no) = let ns' = Map.foldMapWithKey p' ns
                                       in VertexData x ns' (filter (`Map.member` ns') no)
    -- test if we should retain the edge; i.e. if the edge is marked and the
    -- other endpoint exists in our component
    p' i (retain,x) | retain && NEMap.member i m = Map.singleton i x
                    | otherwise                  = Map.empty


-- | Given a connected plane graph in adjacency list format; convert it into an actual
-- PlaneGraph
toPlaneGraph                 :: (Ord r, Foldable1 f
                                )
                             => proxy s
                             -> GGraph f (Point 2 r) v e -> PlaneGraph s (Point 2 r) () ()
toPlaneGraph proxy (Graph m) = fromAdjacencyLists . fmap f . NEMap.assocs $ m
  where
    f (vi, VertexData v eData neighs) = (vi, v, (\vj -> (vj, eData Map.! vj)) <$> neighs)
