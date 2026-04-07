{-# LANGUAGE UndecidableInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Plane.LowerEnvelope.Connected.Type
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- A Representation of the Lower envelope of planes in Adjacency-list
-- form.
--
--------------------------------------------------------------------------------
module HGeometry.Plane.LowerEnvelope.Connected.Type
  ( MinimizationDiagram(..)
  , asMap
  , Region, RegionF(..)
  , toConvexPolygonIn

  , mapVertices

  --   LowerEnvelope'(LowerEnvelope)
  -- , theUnboundedVertex, boundedVertices
  -- , traverseLowerEnvelope

  -- , singleton

  -- , BoundedVertex
  -- , BoundedVertexF(Vertex)
  -- , location, definers, location2

  -- , UnboundedVertex(UnboundedVertex)
  -- , unboundedVertexId
  -- , HasUnboundedEdges(..)

  -- , EdgeGeometry
  -- , projectedEdgeGeometries, projectedEdgeGeometry


  -- , outgoingUnboundedEdge
  -- , edgeIntersectionLine
  -- , intersectionLine'
  -- , intersectionLineWithDefiners
  -- , EdgeDefiners(..)
  ) where

import Control.Lens
import Control.Subcategory.Functor
import Data.List.NonEmpty (NonEmpty(..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.NonEmpty (NEMap)
import Data.Map.NonEmpty qualified as NEMap
import Data.Monoid (First(..))
import HGeometry.Box
import HGeometry.Cyclic
import HGeometry.Direction
import HGeometry.HalfLine
import HGeometry.Intersection
import HGeometry.LineSegment
import HGeometry.Plane.LowerEnvelope.Connected.Region
import HGeometry.Point
import HGeometry.Point.Either
import HGeometry.Polygon.Convex
import HGeometry.Polygon.Convex.Unbounded
import HGeometry.Polygon.Simple.Class
import HGeometry.Properties
import HGeometry.Vector
import HGeometry.Vector.NonEmpty.Util ()
import GHC.Generics (Generic)
import Control.DeepSeq
import HGeometry.Cone.Intersection

--------------------------------------------------------------------------------
-- * The Minimization Diagram, i.e. the type that we use to represent our
-- lower envelope

-- | A minimization daigram just maps every plane on the lower envelope to the region
-- above which it is minimal. Every plane has at most one such a region.
newtype MinimizationDiagram r vertex plane = MinimizationDiagram (NEMap plane (Region r vertex))
  deriving stock (Show,Eq,Generic)
  deriving newtype (NFData)

type instance NumType   (MinimizationDiagram r vertex plane) = r
type instance Dimension (MinimizationDiagram r vertex plane) = 2


instance Constrained (MinimizationDiagram r vertex) where
  type Dom (MinimizationDiagram r vertex) plane = ( Ord plane, NumType plane ~ r
                                                  -- , NumType vertex ~ r
                                                  )

instance CFunctor (MinimizationDiagram r vertex) where
  cmap f (MinimizationDiagram m) = MinimizationDiagram $ NEMap.mapKeys f m

-- | Get the underlying Map that relates every plane in the envelope to its projected region
asMap                         :: MinimizationDiagram r vertex plane
                              -> NEMap plane (Region r vertex)
asMap (MinimizationDiagram m) = m


-- | Apply some mapping function to the vertex data of each vertex
mapVertices                           :: (vertex -> vertex')
                                      -> MinimizationDiagram r vertex plane
                                      -> MinimizationDiagram r vertex' plane
mapVertices f (MinimizationDiagram m) = MinimizationDiagram $ fmap (fmap f) m

-- -- | Apply some mapping function to both the vertices and the planes.
-- cbimap :: (Ord plane, Ord plane', NumType plane ~ r, NumType plane' ~ r)
--        => (vertex -> vertex') -> (plane -> plane')
--        -> MinimizationDiagram r vertex plane
--        -> MinimizationDiagram r vertex' plane'
-- cbimap f g (MinimizationDiagram m) = MinimizationDiagram . fmap (fmap f) $ NEMap.mapKeys g m



--------------------------------------------------------------------------------

-- | Computes a convex polygon corresponding to the region.
--
-- pre: the bounding box (strictly) contains all vertices in its interior
toConvexPolygonIn      :: ( Rectangle_ rectangle corner, Point_ corner 2 r
                          , Point_ point 2 r, Ord r, Fractional r
                          )
                       => rectangle -> Region r point
                       -> Either (ConvexPolygonF (Cyclic NonEmpty) point)
                                 (ConvexPolygonF (Cyclic NonEmpty) (OriginalOrExtra point (Point 2 r)))
toConvexPolygonIn rect = \case
    BoundedRegion convex                -> Left convex
    UnboundedRegion (Unbounded u pts v) ->
                         let p        = NonEmpty.head pts
                             q        = NonEmpty.last pts
                             hp       = HalfLine (p^.asPoint) ((-1) *^ u)
                             hq       = HalfLine (q^.asPoint) v
                             extras   = extraPoints hp hq $ Box (rect^.minPoint.asPoint)
                                                                (rect^.maxPoint.asPoint)
                         in Right . uncheckedFromCCWPoints $
                                (Extra <$> extras) <> (Original <$> pts)

--------------------------------------------------------------------------------

-- instance HasVertices' (MinimizationDiagram r plane) where
--   -- | invariant: vertexIx == UnboundedVertex <=> vertex = UnboundedVertex
--   type VertexIx (MinimizationDiagram r plane) = MDVertexIx
--   type Vertex (MinimizationDiagram r plane)   = MDVertex r plane
--   vertexAt = \case
--     UnboundedVertexIx -> pure UnboundedVertex
--     BoundedVertexIx i ->

-- instance HasVertices (MinimizationDiagram r plane) (MinimizationDiagram r plane) where
-- instance HasDarts' (MinimizationDiagram r plane) where
--   type DartIx (MinimizationDiagram r plane) = (MDVertexIx, MDVertexIx)
--   type Dart (MinimizationDiagram r plane)   = (MDVertex r plane, MDVertex r plane)
--   dartAt = undefined

-- instance HasDarts (MinimizationDiagram r plane) (MinimizationDiagram r plane) where

-- instance DiGraph_ (MinimizationDiagram r plane) where
--   diGraphFromAdjacencyLists = undefined
--   endPoints = undefined
--   outNeighboursOf = undefined
--   twinDartOf = undefined
