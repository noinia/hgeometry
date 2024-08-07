{-# LANGUAGE  UndecidableInstances  #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Plane.LowerEnvelope.Separator.Util
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Utilities for computing a separator for a planar graph
--
--------------------------------------------------------------------------------
module HGeometry.Plane.LowerEnvelope.Connected.Separator.Util
  ( graphEdges
  , treeEdges
  ) where

import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Tree
import           HGeometry.Plane.LowerEnvelope.Connected.Graph


--------------------------------------------------------------------------------

-- | Collect the set of edges; all edges are oriented from small to large.
graphEdges :: Ord k => PlaneGraph k v e -> Set (k,k)
graphEdges = Map.foldMapWithKey (\u (es,_) -> Set.fromList [ (u,v) | v <- Map.elems es, u <= v])

-- | Generate the set of tree edges, all edges are oriented from small to large.
treeEdges              :: Ord k => Tree k -> Set (k,k)
treeEdges (Node u chs) = Set.fromList [ orient (u,v) | Node v _ <- chs ]
                      <> foldMap treeEdges chs
  where
    orient (a,b) = if a <= b then (a,b) else (b,a)
