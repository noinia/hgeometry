{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
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
  ( HasVertices(..), HasVertices'(..)
  , HasEdges(..)

  , Neighbours(..)

  , HasOuterBoundary(..)
  , Polygon_(..)

  , numVertices, numEdges, numFaces
  ) where

import           Control.Lens
import qualified Data.Foldable as F
import           Geometry.Point.Class
import           Geometry.Vector

--------------------------------------------------------------------------------

class HasVertices graph graph' where
  type Vertex   graph
  type VertexIx graph

  -- | Traversal of all vertices in the graph
  vertices :: IndexedTraversal (VertexIx graph) graph graph' (Vertex graph) (Vertex graph')

class HasVertices' graph where
  -- | Traversal of all vertices in the graph, non type changing.
  vertices' :: IndexedTraversal' (VertexIx graph) graph (Vertex graph)
  default vertices'
    :: HasVertices graph graph => IndexedTraversal' (VertexIx graph) graph (Vertex graph)
  vertices' = vertices

  -- | Accessor to a given vertex.
  vertexAt :: VertexIx graph
           -> IndexedTraversal' (VertexIx graph) graph (Vertex graph)


class HasEdges graph graph' where
  type Edge   graph
  type EdgeIx graph

  -- | Traversal of all edges in the graph
  edges :: IndexedTraversal (EdgeIx graph) graph graph' (Edge graph) (Edge graph')


class HasFaces graph graph' where
  type Face   graph
  type FaceIx graph

  -- | Traversal of all faces in the graph
  faces :: IndexedTraversal (FaceIx graph) graph graph' (Face graph) (Face graph')



newtype Neighbours graph = Neighbours (IndexedFold (VertexIx graph) graph (Vertex graph))

class HasAdjacencies graph where
  -- | The neighbours of all vertices in the graph
  adjacencyLists :: IndexedFold (VertexIx graph) graph (Neighbours graph)

  -- | The neighbours of a particular vertex u
  neighboursOf   :: VertexIx graph -> Neighbours graph

--------------------------------------------------------------------------------

numVertices :: HasVertices graph graph => graph -> Int
numVertices = lengthOf vertices

numEdges :: HasEdges graph graph => graph -> Int
numEdges = lengthOf edges

numFaces :: HasFaces graph graph => graph -> Int
numFaces = lengthOf faces


-- ^ A class for items that have an outer boundary.
class HasVertices' polygon => HasOuterBoundary polygon where
  -- | A fold over all vertices of the outer boundary, the
  -- vertices are traversed in CCW order.
  --
  -- running time :: \(O(n)\)
  outerBoundary :: IndexedFold (VertexIx polygon) polygon (Vertex polygon)

  -- | A particular vertex of the outer polygon
  --
  -- running time: \(O(1)\)
  outerBoundaryVertexAt   :: VertexIx polygon
                          -> IndexedGetter (VertexIx polygon) polygon (Vertex polygon)

  -- | A fold over all edges in the polygon. The edges are folded over
  -- in CCW order, and each edge is associated with the index of its start vertex
  -- outerBoundaryEdges :: IndexedFold (VertexIx polygon) polygon (Vertex polygon, Vertex polygon)
  --
  --
  -- running time :: \(O(n)\)
  outerBoundaryEdges :: IndexedFold (VertexIx polygon) polygon (Vertex polygon, Vertex polygon)


  default outerBoundaryEdges
    :: Enum (VertexIx polygon)
    => IndexedFold (VertexIx polygon) polygon (Vertex polygon, Vertex polygon)
  outerBoundaryEdges = ifolding $
    \pg -> map ( \(i,u) -> (i,(u, pg ^.outerBoundaryVertexAt (succ i))) )
         $ itoListOf outerBoundary pg
    -- this feels much more clunky than it should be somehow.

  -- | Get the edge that has the given vertex as its starting edge
  --
  -- running time: \(O(1)\)
  outerBoundaryEdgeAt   :: VertexIx polygon
                        -> IndexedGetter (VertexIx polygon) polygon
                                         (Vertex polygon, Vertex polygon)

  -- default implementation of outerBoundaryEdge. It achieves the
  -- desired running time when indexing is indeed constant.
  default outerBoundaryEdgeAt :: Enum (VertexIx polygon)
                              => VertexIx polygon
                              -> IndexedGetter (VertexIx polygon) polygon
                                               (Vertex polygon, Vertex polygon)
  outerBoundaryEdgeAt i = ito $
    \pg -> (i, (pg^.outerBoundaryVertexAt i, pg^.outerBoundaryVertexAt (succ i)))

  {-# MINIMAL outerBoundary, outerBoundaryVertexAt #-}



class HasHoles face face' where
  type HoleIx face
  type Hole face
  holes :: IndexedTraversal (HoleIx face) face face' (Hole face) (Hole face')

--------------------------------------------------------------------------------

-- | A class representing (planar) polygons. The edges of the polygon
-- may not intersect.
class ( HasOuterBoundary (polygon point r)
      , Vertex      (polygon point r) ~ point 2 r
      , Point_ point 2 r
      , VertexIx    (polygon point r) ~ Int
      ) => Polygon_ polygon point r where

  -- | The area of a polygon
  --
  -- running time: \(O(n)\)
  area :: Fractional r => polygon point r -> r

--------------------------------------------------------------------------------

class ( Polygon_ multiPolygon point r
      ) => MultiPolygon_ multiPolygon point r where



--------------------------------------------------------------------------------


--------------------------------------------------------------------------------

-- data MultiPG
-- data SimplePG

-- class DerivingStrategyPolygon strat polygon point r | polygon point r -> strat where
--   derivedArea :: Fractional r => polygon point r -> r

-- instance (SimplePolygon_ polygon point r) =>  DerivingStrategyPolygon SimplePG polygon point r where
--   derivedArea = signedArea -- abs . signedArea
--                 -- since the polygon is stored in CCW order, there is no need to actually
--                 -- use the absolute value.
