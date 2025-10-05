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
  ( VoronoiDiagram, VoronoiDiagram_(..)
  , VoronoiDiagram'(..)
  , asMap
  , voronoiDiagram
  , voronoiDiagramWith
  , voronoiDiagramWith'
  , voronoiVertices
  -- , edgeGeometries
  , pointToPlane
  ) where

import Control.Lens
import Control.Subcategory.Functor
import Data.Bifunctor
import Data.Foldable1
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map (Map)
import Data.Map.NonEmpty qualified as NEMap
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Vector qualified as Vector
import HGeometry.Duality
import HGeometry.Ext
import HGeometry.HyperPlane.Class
import HGeometry.HyperPlane.NonVertical
import HGeometry.Line.General
import HGeometry.Plane.LowerEnvelope ( MinimizationDiagram, Region(..)
                                     , lowerEnvelope, LowerEnvelope(..)
                                     , MDVertex(..), mapVertices
                                     , VertexForm
                                     , lowerEnvelopeWith, connectedLowerEnvelopeWith
                                     )
import HGeometry.Plane.LowerEnvelope qualified as LowerEnvelope
import HGeometry.Point
import HGeometry.Properties
import HGeometry.Sequence.Alternating (Alternating(..))
import HGeometry.Polygon.Convex.Unbounded
import GHC.Generics (Generic)
import Control.DeepSeq

--------------------------------------------------------------------------------

-- | A Voronoi diagram
type VoronoiDiagram point vtxData = VoronoiDiagram_ (NumType point) point vtxData

data VoronoiDiagram_ r point vtxData =
    AllColinear !(Alternating Vector.Vector (VerticalOrLineEQ r) point)
  | ConnectedVD !(VoronoiDiagram' (MDVertex r point vtxData) point)
  deriving stock (Generic)
--               (NumType point) (Point 2 (NumType point))

    -- Point 2 (NumType point)) )


deriving instance (Show point, Show r, Num r, Show vtxData
                  , Point_ point 2 r
                  ) => Show (VoronoiDiagram_ r point vtxData)
deriving instance (Eq point, Eq (NumType point), Eq r, Eq vtxData
                  ) => Eq   (VoronoiDiagram_ r point vtxData)

instance (NFData point, NFData r, NFData vtxData, NFData (NumType point)
         ) => NFData (VoronoiDiagram_ r point vtxData)


type instance NumType   (VoronoiDiagram_ r point vtxData) = r
type instance Dimension (VoronoiDiagram_ r point vtxData) = 2 -- Dimension point


--------------------------------------------------------------------------------

-- | A connected VoronoiDiagram
newtype VoronoiDiagram' vertex point =
  VoronoiDiagram (MinimizationDiagram (NumType point) vertex point)
  deriving stock (Generic)

deriving instance ( Show point, Show vertex, Show (NumType point)
                  , Point_ point 2 (NumType point)
                  , Point_ vertex 2 (NumType point)
                  ) => Show (VoronoiDiagram' vertex point)
deriving instance (Eq point, Eq vertex, Eq (NumType point))     => Eq   (VoronoiDiagram' vertex point)

deriving instance (NFData (NumType point), NFData vertex, NFData point
                  ) => NFData (VoronoiDiagram' vertex point)


type instance NumType   (VoronoiDiagram' vertex point) = NumType point
type instance Dimension (VoronoiDiagram' vertex point) = 2 -- Dimension point

-- | Iso to Access the underlying LowerEnvelope
_VoronoiDiagramLowerEnvelope :: Iso (VoronoiDiagram' vertex point) (VoronoiDiagram' vertex point')
                                    (MinimizationDiagram (NumType point) vertex point)
                                    (MinimizationDiagram (NumType point') vertex point')
_VoronoiDiagramLowerEnvelope = coerced

-- | Get, for each point, its Voronoi region
asMap :: (Point_ point 2 r, Ord point)
      => VoronoiDiagram' vertex point -> NEMap.NEMap point (Region r vertex)
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
                  ) => f point -> VoronoiDiagram point ()
voronoiDiagram = voronoiDiagramWith lowerEnvelope

-- | Given a function to compute a lower envelope; construct use it to construct the
-- Voronoi diagram.
voronoiDiagramWith :: ( Point_ point 2 r, Functor nonEmpty, Ord point
                      , Ord r, Fractional r, Foldable1 nonEmpty
                      )
                   => (nonEmpty (Plane r :+ point) -> LowerEnvelope (Plane r :+ point) vtxData)

                   -> nonEmpty point
                   -> VoronoiDiagram point vtxData
voronoiDiagramWith lowerEnv pts = case lowerEnv . fmap (\p -> pointToPlane p :+ p) $ pts of
    ParallelStrips strips -> AllColinear $ fmap (^.extra) strips
    ConnectedEnvelope env ->
      ConnectedVD . VoronoiDiagram . mapVertices (first (^.extra)) . cmap (^.extra) $ env

-- | Given a function that can construct the vertices of the lower envelope; use it to
-- construct the Voronoi Diagram.
voronoiDiagramWith'          :: ( Point_ point 2 r, Ord point, Functor set, Foldable1 set
                                , Ord r, Fractional r
                                , Show point, Show r -- TODO: remove
                                )
                             => (set (Plane r :+ point) -> VertexForm Map r (Plane r :+ point))
                             -> set point -> VoronoiDiagram point ()
voronoiDiagramWith' lowerEnv =
  voronoiDiagramWith (lowerEnvelopeWith . connectedLowerEnvelopeWith $ lowerEnv)


-- | lifts the point to a plane; so that the lower envelope corresponds to the VD
pointToPlane :: (Point_ point 2 r, Num r) => point -> Plane r
pointToPlane = flipZ . liftPointToPlane
  where
    flipZ = over (hyperPlaneCoefficients.traverse) negate



-- | Compute the vertices of the Voronoi diagram
voronoiVertices    :: ( Point_ point 2 r, Functor f, Ord point
                      , Ord r, Fractional r, Foldable1 f
                      , Show point, Show r
                      , Ord point
                      ) => f point -> Set (Point 2 r)
voronoiVertices pts = case voronoiDiagram pts of
    AllColinear _  -> mempty
    ConnectedVD vd -> let vd' = vd&coerced %~ mapVertices (^.asPoint)
                      in flip foldMap (asMap vd') $ \case
      BoundedRegion vs       -> foldMap Set.singleton vs
      UnboundedRegion (Unbounded _ vs _) -> Set.fromList (NonEmpty.toList vs)

--------------------------------------------------------------------------------

-- -- | Get the halflines and line segments representing the VoronoiDiagram
-- edgeGeometries :: (Point_ point 2 r, Ord r, Fractional r

--                   , Show point, Show r
--                   )
--                => Fold (VoronoiDiagram' point) (EdgeGeometry (Point 2 r))
-- edgeGeometries = _VoronoiDiagramLowerEnvelope.projectedEdgeGeometries
-- -- TODO: figure out if this can be an indexed fold
