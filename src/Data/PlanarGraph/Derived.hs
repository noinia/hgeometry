{-# LANGUAGE ScopedTypeVariables #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Data.PlanarGraph.Derived
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Some higher level functions dealing with planar graphs
--------------------------------------------------------------------------------
module Data.PlanarGraph.Derived where

import           Control.Lens hiding ((.=))
import           Control.Monad.State.Strict
import           Data.Bifunctor
import           Data.Ext
import qualified Data.Foldable as F
import           Data.Permutation
import           Data.PlanarGraph.Core
import           Data.PlanarGraph.Dart
import           Data.PlanarGraph.EdgeOracle

--------------------------------------------------------------------------------


-- | Construct a planar graph from a adjacency matrix. For every vertex, all
-- vertices should be given in counter clockwise order.
--
-- pre: No self-loops, and no multi-edges
--
-- running time: \(O(n)\).
fromAdjacencyLists      :: forall s w h. (Foldable h, Functor h)
                        => [(VertexId s w, h (VertexId s w))]
                        -> PlanarGraph s w () () ()
fromAdjacencyLists adjM = planarGraph' . toCycleRep n $ perm
  where
    n    = sum . fmap length $ perm
    perm = map toOrbit  $ adjM'

    adjM' = fmap (second F.toList) adjM

    -- -- | Assign Arcs
    -- adjM' = (^._1) . foldr assignArcs (SP [] 0) $ adjM

    -- Build an edgeOracle, so that we can query the arcId assigned to
    -- an edge in O(1) time.
    oracle :: EdgeOracle s w Int
    oracle = fmap (^.core) . assignArcs . buildEdgeOracle
           . map (second $ map ext)  $ adjM'

    toOrbit (u,adjU) = concatMap (toDart u) adjU

    -- if u = v we have a self-loop, so we add both a positive and a negative dart
    toDart u v = let Just a = findEdge u v oracle
                 in case u `compare` v of
                      LT -> [Dart (Arc a) Positive]
                      EQ -> [Dart (Arc a) Positive, Dart (Arc a) Negative]
                      GT -> [Dart (Arc a) Negative]


assignArcs   :: EdgeOracle s w e -> EdgeOracle s w (Int :+ e)
assignArcs o = evalState (traverse f o) 0
  where
    f   :: e -> State Int (Int :+ e)
    f e = do i <- get ; put (i+1) ; pure (i :+ e)
