--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometryPlaneGraph.Class
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Type type for planar graphs embedded in \(\mathbb{R}^2\). For functions that
-- export faces and edges etc, we assume the graph has a (planar) straight line
-- embedding.
--
--------------------------------------------------------------------------------
module HGeometry.PlaneGraph.Class
  ( PlaneGraph_
  , HasLocation(..)
  -- , VertexData(VertexData), location
  ) where

import           Control.Lens
import           HGeometry.Point
import           HGeometry.Properties
import           Hiraffe.Graph.Class

--------------------------------------------------------------------------------

type PlaneGraph_ graph = ( PlanarGraph_ graph
                         , HasLocation (Vertex graph) (Vertex graph)
                         , Dimension (Vertex graph) ~ 2
                         )

class HasLocation s t where
  -- | Lens to access the location
  location :: Lens s t (Point (Dimension s) (NumType s)) (Point (Dimension t) (NumType t))
