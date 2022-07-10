{-# LANGUAGE DefaultSignatures #-}
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

  , numVertices, numEdges, numFaces
  ) where

import           Control.Lens
import qualified Data.Foldable as F

--------------------------------------------------------------------------------

class HasVertices graph graph' where
  type Vertex   graph
  type VertexIx graph

  -- | Traversal of all vertices in the graph
  vertices :: IndexedTraversal (VertexIx graph) graph graph' (Vertex graph) (Vertex graph')

class HasVertices' graph where
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


class HasVertices' polygon => HasOuterBoundary polygon where
  -- | A fold over all vertices of the outer boundary, the
  -- vertices are traversed in CCW order.
  outerBoundary :: IndexedFold (VertexIx polygon) polygon (Vertex polygon)

  -- | A particular vertex of the outer polygon
  outerBoundaryVertexAt   :: VertexIx polygon
                          -> IndexedGetter (VertexIx polygon) polygon (Vertex polygon)

  -- | A fold over all edges in the polygon. The edges are folded over
  -- in CCW order, and each edge is associated with the index of its start vertex
  -- outerBoundaryEdges :: IndexedFold (VertexIx polygon) polygon (Vertex polygon, Vertex polygon)
  outerBoundaryEdges :: IndexedFold (VertexIx polygon) polygon (Vertex polygon, Vertex polygon)

  default outerBoundaryEdges
    :: Enum (VertexIx polygon)
    => IndexedFold (VertexIx polygon) polygon (Vertex polygon, Vertex polygon)
  outerBoundaryEdges = ifolding $
    \pg -> map ( \(i,u) -> (i,(u, pg ^.outerBoundaryVertexAt (succ i))) )
         $ itoListOf outerBoundary pg
    -- this feels much more clunky than it should be somehow.

  -- | Get the edge that has the given vertex as its starting edge
  outerBoundaryEdgeAt   :: VertexIx polygon
                        -> IndexedGetter (VertexIx polygon) polygon
                                         (Vertex polygon, Vertex polygon)

  -- default implementation of outerBoundaryEdge
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


class Polygon_ polygon point r where
  area :: Fractional r => polygon point r -> r


  -- area = abs $ signedArea poly

signedArea :: polygon point r -> r
signedArea = undefined

-- centroid :: polygon point r -> point 2 r



-- pickPoint :: polygon point r -> point d r


-- isTriangle :: simplePolygon point r -> bool


-- signedArea      :: (Fractional r, SimplePolygon_ simplePolygon point r)
--                 => simplepolygon point r -> r
-- signedArea poly = signedArea2X poly / 2

-- -- | Compute the signed area times 2 of a simple polygon. The the vertices are in
-- -- clockwise order, the signed area will be negative, if the verices are given
-- -- in counter clockwise order, the area will be positive.
-- signedArea2X      :: (Num r, SimplePolygon_ simplePolygon point r)
--                   => simplepolygon p r -> r
-- signedArea2X poly = x
--   where
--     x = sum [ p^.core.xCoord * q^.core.yCoord - q^.core.xCoord * p^.core.yCoord
--             | LineSegment' p q <- F.toList $ outerBoundaryEdges poly  ]
