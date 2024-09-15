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
  , ColinearPoint
  , voronoiDiagram
  , voronoiVertices
  , edgeGeometries
  ) where

import           Control.Lens
import           Data.Foldable1
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           HGeometry.Box
import           HGeometry.Duality
import           HGeometry.Ext
import           HGeometry.HyperPlane.Class
import           HGeometry.HyperPlane.NonVertical
import           HGeometry.Plane.LowerEnvelope.Connected.Regions
import           HGeometry.Plane.LowerEnvelope.Naive (lowerEnvelope, LowerEnvelope(..))
import           HGeometry.Point
import           HGeometry.Properties
--------------------------------------------------------------------------------

-- | A Voronoi diagram
data VoronoiDiagram point = AllColinear !(Set.Set (ColinearPoint point))
                          | ConnectedVD !(VoronoiDiagram' point)

deriving instance (Show point, Show (NumType point)) => Show (VoronoiDiagram point)
deriving instance (Eq point, Eq (NumType point))     => Eq   (VoronoiDiagram point)

type instance NumType   (VoronoiDiagram point) = NumType point
type instance Dimension (VoronoiDiagram point) = 2 -- Dimension point

-- | Just a newtype around point; used to model a set of points that are all colinear in
-- the Vornoi diagram.
newtype ColinearPoint point = ColinearPoint point
                       deriving (Show,Eq)

instance Wrapped (ColinearPoint point) where
  type Unwrapped (ColinearPoint point)  = point
  _Wrapped' = coerced

-- instance Rewrapped (ColinearPoint point) point

--------------------------------------------------------------------------------

-- | A connected VoronoiDiagram
newtype VoronoiDiagram' point = VoronoiDiagram (MinimizationDiagram (NumType point) point)

deriving instance (Show point, Show (NumType point)) => Show (VoronoiDiagram' point)
deriving instance (Eq point, Eq (NumType point))     => Eq   (VoronoiDiagram' point)

type instance NumType   (VoronoiDiagram' point) = NumType point
type instance Dimension (VoronoiDiagram' point) = 2 -- Dimension point

-- | Iso to Access the underlying LowerEnvelope
_VoronoiDiagramLowerEnvelope :: Iso (VoronoiDiagram' point) (VoronoiDiagram' point')
                                    (MinimizationDiagram (NumType point) point)
                                    (MinimizationDiagram (NumType point') point')
_VoronoiDiagramLowerEnvelope = coerced

--------------------------------------------------------------------------------

-- instance (Ord (NumType point), Num (NumType point)) => IsBoxable (VoronoiDiagram' point) where
--   boundingBox vd = projectPoint <$> boundingBox (vd^._VoronoiDiagramLowerEnvelope)

--------------------------------------------------------------------------------

-- | Computes the Voronoi Diagram, by lifting the points to planes, and computing
-- the lower envelope of these planes.
--
-- O(n^4) ( we currently use the brute force implentation...) TODO: switch to something faster

-- \(O(n\log n)\)
voronoiDiagram :: ( Point_ point 2 r, Functor f, Ord point
                  , Ord r, Fractional r, Foldable1 f
                  , Show point, Show r
                  ) => f point -> VoronoiDiagram point
voronoiDiagram = voronoiDiagramWith lowerEnvelope

-- | Given a function to compute a lower envelope; construct use it to construct the
-- Voronoi diagram.
voronoiDiagramWith :: ( Point_ point 2 r, Functor nonEmpty, Ord point
                      , Ord r, Fractional r, Foldable1 nonEmpty
                      )
                   => (nonEmpty (Plane 3 r :+ point) -> LowerEnvelope r (Plane 3 r :+ point))

                   -> nonEmpty point
                   -> VoronoiDiagram point
voronoiDiagramWith lowerEnv pts = case lowerEnv . fmap (\p -> pointToPlane p :+ p) $ pts of
    ParallelStrips strips -> AllColinear $ Set.mapMonotonic getPoint strips
    ConnectedEnvelope env -> ConnectedVD . VoronoiDiagram . Map.mapKeys (view extra) $ env
  where
    -- lifts the point to a plane; so that the lower envelope corresponds to the VD
    pointToPlane = flipZ . liftPointToPlane
    flipZ = over (hyperPlaneCoefficients.traverse) negate

    getPoint = view (_Wrapped'.extra.to ColinearPoint)


-- | Compute the vertices of the Voronoi diagram
voronoiVertices :: ( Point_ point 2 r, Functor f, Ord point
                   , Ord r, Fractional r, Foldable1 f
                   , Show point, Show r
                   , Ord point
                   ) => f point -> Set (Point 2 r)
voronoiVertices = foldMap (\case
                              Bounded pts       -> Set.fromList pts
                              Unbounded _ pts _ -> Set.fromList (NonEmpty.toList pts)
                          ) . voronoiDiagram'

--------------------------------------------------------------------------------

-- | Get the halflines and line segments representing the VoronoiDiagram
edgeGeometries :: (Point_ point 2 r, Ord r, Fractional r

                  , Show point, Show r
                  )
               => Fold (VoronoiDiagram' point) (EdgeGeometry (Point 2 r))
edgeGeometries = _VoronoiDiagramLowerEnvelope.projectedEdgeGeometries
-- TODO: figure out if this can be an indexed fold
