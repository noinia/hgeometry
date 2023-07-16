--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.VoronoiDiagram.ViaLowerEnvelope
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Compute a Voronoi Diagram of a set of points in R^2 using the lower
-- envelope of planes in R^3.
--
--------------------------------------------------------------------------------
module HGeometry.VoronoiDiagram.ViaLowerEnvelope
  ( voronoiVertices
  ) where

import           Control.Lens
import           Data.Default.Class
import qualified Data.Map as Map
import           HGeometry.Duality
import           HGeometry.Ext
import           HGeometry.HyperPlane.Class
import           HGeometry.HyperPlane.NonVertical
import           HGeometry.LowerEnvelope.AdjListForm
import           HGeometry.LowerEnvelope.Naive (lowerEnvelopeVertexForm)
import           HGeometry.Point
import           HGeometry.Properties
import           Hiraffe.Graph

--------------------------------------------------------------------------------

type VoronoiDiagram point = LowerEnvelope (Plane (NumType point) :+ point)



-- | Computes all voronoi vertices
voronoiVertices :: ( Point_ point 2 r, Functor f, Default point, Ord point
                   , Ord r, Fractional r, Foldable f
                   ) => f point -> [Point 2 r]
voronoiVertices = map (projectPoint . fst)
                . itoListOf vertices
                . lowerEnvelopeVertexForm
                . fmap (\p -> liftPointToPlane p :+ p)
-- FIXME: get rid of the default point constraint
