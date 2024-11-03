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
  , Region(..)
  , CircularList


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

import           Control.Subcategory.Functor
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Map (Map)
import qualified Data.Map as Map
import           HGeometry.Point
import           HGeometry.Properties
import           HGeometry.Vector
import           HGeometry.Vector.NonEmpty.Util ()

--------------------------------------------------------------------------------
-- * The Minimization Diagram, i.e. the type that we use to represent our
-- lower envelope

-- | A minimization daigram just maps every plane on the lower envelope to the region
-- above which it is minimal. Every plane has at most one such a region.
newtype MinimizationDiagram r plane = MinimizationDiagram (Map plane (Region r (Point 2 r)))
  deriving stock (Show,Eq)

type instance NumType   (MinimizationDiagram r plane) = r
type instance Dimension (MinimizationDiagram r plane) = 2

instance Constrained (MinimizationDiagram r) where
  type Dom (MinimizationDiagram r) plane = (Ord plane, NumType plane ~ r)

instance CFunctor (MinimizationDiagram r) where
  cmap f (MinimizationDiagram m) = MinimizationDiagram $ Map.mapKeys f m



-- | Get the underlying Map that relates every plane in the envelope to its projected region
asMap                         :: MinimizationDiagram r plane -> Map plane (Region r (Point 2 r))
asMap (MinimizationDiagram m) = m



-- | A region in the minimization diagram. The boundary is given in CCW order; i.e. the
-- region is to the left of the boundary.
data Region r point = Bounded   (CircularList point)
                    | Unbounded (Vector 2 r)
                                -- ^ vector indicating the direction of the unbounded edge
                                -- incident to the first vertex. Note that this vector
                                -- thus points INTO vertex v.
                                (NonEmpty point)
                                -- ^ the vertices in CCW order,
                                (Vector 2 r)
                                -- ^ the vector indicating the direction of the unbounded
                                -- edge incident to the last vertex. The vector points
                                -- away from the vertex (i.e. towards +infty).
                      deriving stock (Show,Eq,Functor,Foldable,Traversable)

type instance NumType   (Region r point) = r
type instance Dimension (Region r point) = Dimension point

-- | bounded regions are really circular lists, but we just represent them as lists for
-- now.
type CircularList a = [a]


--------------------------------------------------------------------------------

-- data MDVertexIx = UnboundedVertexIx
--                 | BoundedVertexIx {-# UNPACK #-}!Int
--                 deriving (Show,Read,Eq,Ord)

-- data MDVertex r plane = UnboundedVertex
--                       | BoundedVertex { _location :: Point 2 r
--                                       , _definers :: Vector 3 plane
--                                       }
--                       deriving (Show,Eq,Ord)


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


-- -- | Produce a triangulated plane graph on the bounded vertices.  every vertex is
-- -- represented by its point, it stores a list of its outgoing edges, and some data.
-- toGraph :: (Plane_ plane r, Num r, Ord r)
--         => MinimizationDiagram r plane -> PlaneGraph (Point 2 r) (First r) (E r)
-- toGraph = mapWithKeyMerge toTriangulatedGr . asMap
