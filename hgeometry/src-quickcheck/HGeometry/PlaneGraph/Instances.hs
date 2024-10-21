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

import Data.Proxy
import qualified Data.Set as Set
import Data.Set(Set)
import Data.Tree
import HGeometry.Instances ()
import HGeometry.PlaneGraph
import HGeometry.Point
import HGeometry.VoronoiDiagram
import Hiraffe.BFS.Pure
import Test.QuickCheck
import Test.QuickCheck.Instances ()

--------------------------------------------------------------------------------

data QuickCheckWorld

instance Arbitrary r => Arbitrary (PlaneGraph QuickCheckWorld (Point 2 r) () ()) where
  arbitrary = arbitraryPlaneGraph Proxy

-- general strategy:
-- 1) generate a bunch of random points uniformly at random.
-- 2) Construct the Voronoi diagram
-- 3) construct a spanning tree on the Voronoi vertices (so that the graph is connected)
-- 4) randomly include additional (bounded) edges
-- 5) construct the plane graph from the connected set of line segments/edgse

arbitraryPlaneGraph       :: proxy s -> Gen (PlaneGraph s (Point 2 r) () ())
arbitraryPlaneGraph proxy = do
    pts <- arbitrary
    case voronoiDiagram pts of
      AllColinear _  -> arbitraryPlaneGraph proxy -- retry
      ConnectedVD vd -> let gr = undefined in case bff gr of
        []               -> error "arbitraryPlaneGraph. absurd"
        (spanningTree:_) -> fromSubsetOfEdges proxy gr . includeTreeEdges spanningTree
                         <$> sublistOf (gr^..edges)

fromSubsetOfEdges          :: proxy s -> graph -> PlaneGraph s (Point 2 r) () ()
fromSubsetOfEdges proxy gr = fromAdjacencyLists $ undefined

includeTreeEdges    :: Tree (VertexIx graph) -> [EdgeIx graph] -> Set (EdgeIx graph)
includeTreeEdges tr = undefined
