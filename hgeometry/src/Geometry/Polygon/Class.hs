--------------------------------------------------------------------------------
-- |
-- Module      :  Geometry.Polygon.Class
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- A polygon and some basic functions to interact with them.
--
--------------------------------------------------------------------------------
module Geometry.Polygon.Class
  ( HasVertices(..)
  , HasEdges(..)
  ) where

import           Control.Lens

--------------------------------------------------------------------------------

class HasVertices graph graph' where
  type Vertex   graph
  type VertexIx graph
  vertices :: IndexedTraversal (VertexIx graph) graph graph' (Vertex graph) (Vertex graph')


class HasEdges graph graph' where
  type Edge   graph
  type EdgeIx graph
  edges :: IndexedTraversal (EdgeIx graph) graph graph' (Edge graph) (Edge graph')



--------------------------------------------------------------------------------
