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
  , asMap
  , voronoiDiagram
  , voronoiVertices
  -- , edgeGeometries
  ) where

import           Control.Lens
import           Control.Subcategory.Functor
import           Data.Foldable1
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Vector as Vector
import           HGeometry.Duality
import           HGeometry.Ext
import           HGeometry.HyperPlane.Class
import           HGeometry.HyperPlane.NonVertical
import           HGeometry.Line.General
import           HGeometry.Plane.LowerEnvelope ( MinimizationDiagram, Region(..)
                                               , lowerEnvelope, LowerEnvelope(..)
                                               )
import qualified HGeometry.Plane.LowerEnvelope as LowerEnvelope
import           HGeometry.Point
import           HGeometry.Properties
import           HGeometry.Sequence.Alternating (Alternating(..))

--------------------------------------------------------------------------------

-- | A Voronoi diagram
data VoronoiDiagram point =
    AllColinear !(Alternating Vector.Vector (VerticalOrLineEQ (NumType point)) point)
  | ConnectedVD !(VoronoiDiagram' point)

deriving instance (Show point, Show (NumType point)) => Show (VoronoiDiagram point)
deriving instance (Eq point, Eq (NumType point))     => Eq   (VoronoiDiagram point)

type instance NumType   (VoronoiDiagram point) = NumType point
type instance Dimension (VoronoiDiagram point) = 2 -- Dimension point


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

-- | Get, for each point, its Voronoi region
asMap :: (Point_ point 2 r, Ord point)
      => VoronoiDiagram' point -> Map.Map point (Region r (Point 2 r))
asMap = LowerEnvelope.asMap . view _VoronoiDiagramLowerEnvelope

--------------------------------------------------------------------------------

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
                   => (nonEmpty (Plane r :+ point) -> LowerEnvelope (Plane r :+ point))

                   -> nonEmpty point
                   -> VoronoiDiagram point
voronoiDiagramWith lowerEnv pts = case lowerEnv . fmap (\p -> pointToPlane p :+ p) $ pts of
    ParallelStrips strips -> AllColinear $ fmap (^.extra) strips
    ConnectedEnvelope env -> ConnectedVD . VoronoiDiagram . cmap (^.extra) $ env
  where
    -- lifts the point to a plane; so that the lower envelope corresponds to the VD
    pointToPlane = flipZ . liftPointToPlane
    flipZ = over (hyperPlaneCoefficients.traverse) negate



-- | Compute the vertices of the Voronoi diagram
voronoiVertices    :: ( Point_ point 2 r, Functor f, Ord point
                      , Ord r, Fractional r, Foldable1 f
                      , Show point, Show r
                      , Ord point
                      ) => f point -> Set (Point 2 r)
voronoiVertices pts = case voronoiDiagram pts of
    AllColinear _  -> mempty
    ConnectedVD vd -> foldMap (\case
                                  Bounded vs       -> Set.fromList vs
                                  Unbounded _ vs _ -> Set.fromList (NonEmpty.toList vs)
                              ) (asMap vd)

--------------------------------------------------------------------------------

-- -- | Get the halflines and line segments representing the VoronoiDiagram
-- edgeGeometries :: (Point_ point 2 r, Ord r, Fractional r

--                   , Show point, Show r
--                   )
--                => Fold (VoronoiDiagram' point) (EdgeGeometry (Point 2 r))
-- edgeGeometries = _VoronoiDiagramLowerEnvelope.projectedEdgeGeometries
-- -- TODO: figure out if this can be an indexed fold
