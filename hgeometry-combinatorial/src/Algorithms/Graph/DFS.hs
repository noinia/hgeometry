{-# LANGUAGE ScopedTypeVariables #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Algorithms.Graph.DFS
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--------------------------------------------------------------------------------
module Algorithms.Graph.DFS where

import           Control.Monad.ST            (ST, runST)
import           Data.Maybe
import           Data.PlanarGraph
import           Data.Tree
import qualified Data.Vector                 as V
import qualified Data.Vector.Generic         as GV
import qualified Data.Vector.Unboxed.Mutable as UMV
import qualified Data.IntSet as IntSet

-- | DFS on a planar graph.
--
-- Running time: \(O(n)\)
--
-- Note that since our planar graphs are always connected there is no need need
-- for dfs to take a list of start vertices.
dfs  :: forall s w v e f.
      PlanarGraph s w v e f -> VertexId s w -> Tree (VertexId s w)
dfs g = dfs' (adjacencyLists g)

-- | Adjacency list representation of a graph: for each vertex we simply list
-- all connected neighbours.
type AdjacencyLists s w = V.Vector [VertexId s w]

-- | Transform into adjacencylist representation
adjacencyLists   :: PlanarGraph s w v e f -> AdjacencyLists s w
adjacencyLists g = V.toList . flip neighboursOf g <$> vertices' g

-- | DFS, from a given vertex, on a graph in AdjacencyLists representation.
--
-- Running time: \(O(n)\)
dfs'          :: forall s w. AdjacencyLists s w -> VertexId s w -> Tree (VertexId s w)
dfs' g start = runST $ do
                 bv     <- UMV.replicate n False -- bit vector of marks
                 -- start will be unvisited, thus the fromJust is safe
                 fromJust <$> dfs'' bv start
  where
    n = GV.length g

    neighs              :: VertexId s w -> [VertexId s w]
    neighs (VertexId u) = g GV.! u

    visit   bv (VertexId i) = UMV.write bv i True
    visited bv (VertexId i) = UMV.read  bv i
    dfs''      :: UMV.MVector s' Bool -> VertexId s w
               -> ST s' (Maybe (Tree (VertexId s w)))
    dfs'' bv u = visited bv u >>= \case
                   True  -> pure Nothing
                   False -> do
                              visit bv u
                              Just . Node u . catMaybes <$> mapM (dfs'' bv) (neighs u)

-- | DFS, from a given vertex, on a graph in AdjacencyLists representation. Cycles are not removed.
-- If your graph may contain cycles, see 'dfsFilterCycles'.
--
-- Running time: \(O(k)\), where \(k\) is the number of branches consumed.
dfsSensitive          :: forall s w. (VertexId s w -> [VertexId s w]) -> VertexId s w -> Tree (VertexId s w)
dfsSensitive neighs start =
  dfs'' (_unVertexId start)
 where
  dfs'' :: Int -> Tree (VertexId s w)
  dfs'' u = Node (VertexId u) $ map (dfs'' . _unVertexId) (neighs (VertexId u))

-- | Remove infinite cycles from a DFS search tree.
dfsFilterCycles :: Tree (VertexId s w) -> Tree (VertexId s w)
dfsFilterCycles = worker IntSet.empty
  where
    worker seen (Node root forest) = Node root
      [ Node (VertexId v) (map (worker (IntSet.insert v seen)) sub)
      | Node (VertexId v) sub <- forest
      , v `IntSet.notMember` seen
      ]
