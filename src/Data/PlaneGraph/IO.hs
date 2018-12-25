--------------------------------------------------------------------------------
-- |
-- Module      :  Data.PlaneGraph.IO
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Converting from/to our JSON/Yaml representation of the plane graph
--
--------------------------------------------------------------------------------
module Data.PlaneGraph.IO where

import Data.PlaneGraph.JSON
import Data.PlaneGraph
import Control.Lens
import qualified Data.Foldable as F
import qualified Data.Vector as V

--------------------------------------------------------------------------------


-- | Transforms the planar graph into a format taht can be easily converted
-- into JSON format. For every vertex, the adjacent vertices are given in
-- counter clockwise order.
--
-- See 'toAdjacencyLists' for notes on how we handle self-loops.
--
-- running time: \(O(n)\)
toJSONRep   :: PlaneGraph s v e f r -> Gr (Vtx v e r) (Face f)
toJSONRep g = Gr vs fs
  where
    vs = undefined
    fs = undefined


fromJSONRep                 :: proxy s -> Gr (Vtx v e r) (Face f) -> PlaneGraph s v e f r
fromJSONRep px (Gr as' fs') = undefined
