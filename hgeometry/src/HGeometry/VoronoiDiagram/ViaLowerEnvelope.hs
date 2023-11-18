{-# LANGUAGE UndecidableInstances #-}
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
  ( VoronoiDiagram(..)
  , voronoiDiagram
  , voronoiVertices
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
import           HGeometry.LowerEnvelope.VertexForm (VertexForm)
import           HGeometry.Point
import           HGeometry.Properties
import           HGeometry.Vector
import           Hiraffe.Graph

--------------------------------------------------------------------------------

newtype VoronoiDiagram point =
  VoronoiDiagram (LowerEnvelope (Plane (NumType point) :+ point))

deriving instance (Show point, Show (NumType point)) => Show (VoronoiDiagram point)
deriving instance (Eq point, Eq (NumType point))     => Eq   (VoronoiDiagram point)

--------------------------------------------------------------------------------

-- | Computes the Voronoi Diagram, by lifting the points to planes, and computing
-- the lower envelope of these planes.
--
-- \(O(n\log n)\)
voronoiDiagram :: ( Point_ point 2 r, Functor f, Default point, Ord point
                   , Ord r, Fractional r, Foldable f
                   ) => f point -> VoronoiDiagram point
voronoiDiagram = VoronoiDiagram
               . fromVertexForm
               . upperEnvelopeVertexForm
               . fmap (\p -> liftPointToPlane p :+ p)

-- | Computes all voronoi vertices
voronoiVertices :: ( Point_ point 2 r, Functor f, Default point, Ord point
                   , Ord r, Fractional r, Foldable f
                   ) => f point -> [Point 2 r]
voronoiVertices = map (projectPoint . fst)
                . itoListOf vertices
                . upperEnvelopeVertexForm
                . fmap (\p -> liftPointToPlane p :+ p)
-- FIXME: get rid of the default point constraint
-- FIXME: get rid of the ord point constraint


upperEnvelopeVertexForm :: ( Plane_ plane r
                           , Ord r, Fractional r, Foldable f, Functor f, Ord plane
                           ) => f plane -> VertexForm plane
upperEnvelopeVertexForm = lowerEnvelopeVertexForm . fmap flipZ
  where
    flipZ = over (hyperPlaneCoefficients.traverse) negate

-- FIXME: define this in some individual module
