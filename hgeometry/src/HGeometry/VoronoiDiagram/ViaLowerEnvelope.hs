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
  , VoronoiDiagram'(..)
  , voronoiDiagram
  , voronoiVertices
  , edgeGeometries
  ) where

import           Control.Lens
import           Data.Default.Class
import qualified Data.Map as Map
import           HGeometry.Box
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

-- | A Voronoi diagram
data VoronoiDiagram point = AllColinear () -- TODO
                          | ConnectedVD (VoronoiDiagram' point)


deriving instance (Show point, Show (NumType point)) => Show (VoronoiDiagram point)
deriving instance (Eq point, Eq (NumType point))     => Eq   (VoronoiDiagram point)

type instance NumType   (VoronoiDiagram point) = NumType point
type instance Dimension (VoronoiDiagram point) = 2 -- Dimension point


--------------------------------------------------------------------------------

-- | A connected VoronoiDiagram
newtype VoronoiDiagram' point =
  VoronoiDiagram (LowerEnvelope' (Plane (NumType point) :+ point))

deriving instance (Show point, Show (NumType point)) => Show (VoronoiDiagram' point)
deriving instance (Eq point, Eq (NumType point))     => Eq   (VoronoiDiagram' point)

type instance NumType   (VoronoiDiagram' point) = NumType point
type instance Dimension (VoronoiDiagram' point) = 2 -- Dimension point

-- | Iso to Access the underlying LowerEnvelope
_VoronoiDiagramLowerEnvelope :: Iso (VoronoiDiagram' point) (VoronoiDiagram' point')
                                    (LowerEnvelope' (Plane (NumType point) :+ point))
                                    (LowerEnvelope' (Plane (NumType point') :+ point'))
_VoronoiDiagramLowerEnvelope = coerced

--------------------------------------------------------------------------------

instance (Ord (NumType point), Num (NumType point)) => IsBoxable (VoronoiDiagram' point) where
  boundingBox vd = projectPoint <$> boundingBox (vd^._VoronoiDiagramLowerEnvelope)

--------------------------------------------------------------------------------

-- | Computes the Voronoi Diagram, by lifting the points to planes, and computing
-- the lower envelope of these planes.
--
-- \(O(n\log n)\)
voronoiDiagram     :: ( Point_ point 2 r, Functor f, Default point, Ord point
                      , Ord r, Fractional r, Foldable f
                      , Show point, Show r
                      ) => f point -> VoronoiDiagram point
voronoiDiagram pts = case lowerEnvelope' . fmap (\p -> liftPointToPlane p :+ p) $ pts of
                       ParallelStrips strips -> AllColinear strips -- TODO
                       ConnectedEnvelope env -> ConnectedVD $ VoronoiDiagram env
  where
    lowerEnvelope' = fromVertexForm . upperEnvelopeVertexForm

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




--------------------------------------------------------------------------------

-- | Get the halflines and line segments representing the VoronoiDiagram
edgeGeometries :: (Point_ point 2 r, Ord r, Fractional r, Default point

                  , Show point, Show r
                  )
               => Fold (VoronoiDiagram' point) (EdgeGeometry (Point 2 r))
edgeGeometries = _VoronoiDiagramLowerEnvelope.projectedEdgeGeometries
-- TODO: figure out if this can be an indexed fold
