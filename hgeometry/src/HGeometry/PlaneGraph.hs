--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.PlaneGraph.Connected
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Plane Graphs, i.e. embedded planar graphs.
--
--------------------------------------------------------------------------------
module HGeometry.PlaneGraph
  ( module Hiraffe.Graph.Class
  , module HGeometry.PlaneGraph.Class
  , module Hiraffe.PlanarGraph.Class
  , CPlaneGraph(..)
  , fromAdjacencyRep
  , fromConnectedSegments
  ) where

import HGeometry.PlaneGraph.Class
import HGeometry.PlaneGraph.Connected
import Hiraffe.Graph.Class
import Hiraffe.PlanarGraph.Class


--------------------------------------------------------------------------------
