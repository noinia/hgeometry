--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.PlaneGraph
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Plane Graphs, i.e. embedded planar graphs.
--
--------------------------------------------------------------------------------
module HGeometry.PlaneGraph
  ( module Hiraffe.Graph.Class
    , PlaneGraph
  ) where


import HGeometry.PlaneGraph.Class
import Hiraffe.Graph.Class
import Hiraffe.PlanarGraph

--------------------------------------------------------------------------------

-- | A Plane graph
type PlaneGraph s v e f = PlanarGraph s v e f
