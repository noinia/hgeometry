module Data.PlanarGraph.Persistent where

import Data.HashMap.Strict
import Control.Monad.State

type HalfEdge = Int
type Vertex = Int
type Face = Int

-- PlanarGraphs have vertices, edges, and faces.
-- Invariant: The half-edge of a boundary vertex is interior, twin is exterior.
data PlanarGraph = PlanarGraph
  { pgNextHalfEdgeId     :: HalfEdge
  , pgNextVertexId       :: Vertex
  , pgNextFaceId         :: Face
  , pgNext               :: HashMap HalfEdge HalfEdge
  , pgVertex             :: HashMap HalfEdge Vertex
  , pgFace               :: HashMap HalfEdge Face
  , pgHalfEdgeFromVertex :: HashMap Vertex HalfEdge
  , pgHalfEdgeFromFace   :: HashMap Face HalfEdge
  }

-- data PlaneGraph r = PlaneGraph PlanarGraph (HashMap Vertex (Point 2 r))

new :: Int -> PlanarGraph
new = undefined

-- O(log n)
-- vertexHalfEdge :: Vertex -> PlanarGraph -> HalfEdge
-- O(k log n)
-- vertexOutgoingHalfEdges :: Vertex -> PlanarGraph -> [HalfEdge]
-- O(k log n)
-- vertexIncomingHalfEdges :: Vertex -> PlanarGraph -> [HalfEdge]
-- vertexAdjacentVertices :: Vertex -> PlanarGraph -> [Vertex]
-- vertexAdjacentFaces :: Vertex -> PlanarGraph -> [Face]

halfEdgeNext       :: HalfEdge -> PlanarGraph -> HalfEdge
halfEdgeNext = undefined

halfEdgeNextM       :: MonadState PlanarGraph m => HalfEdge -> m HalfEdge
halfEdgeNextM = gets . halfEdgeNext

-- halfEdgeNext       :: HalfEdge -> PlanarGraph -> HalfEdge
-- halfEdgeVertex     :: HalfEdge -> PlanarGraph -> Vertex
-- halfEdgeTwin       :: HalfEdge -> PlanarGraph -> HalfEdge
-- halfEdgeTailVertex :: HalfEdge -> PlanarGraph -> Vertex
-- halfEdgeTipVertex  :: HalfEdge -> PlanarGraph -> Vertex
-- halfEdgeFace       :: HalfEdge -> PlanarGraph -> Face
-- halfEdgeIsInterior

-- faceHalfEdge :: Face -> PlanarGraph -> HalfEdge
-- faceIsInterior
-- faceIsBoundary
-- faceAdjacentVertices :: 
-- faceAdjaventFaces :: Face -> PlanarGraph -> [Face]

-- connectVertices :: HalfEdge -> HalfEdge -> PlanarGraph -> PlanarGraph
-- splitHalfEdge :: HalfEdge -> PlanarGraph -> (PlanarGraph, Vertex)

-- Use cases:
--   Triangulate polygon.
--     Create PlanarGraph from polygon. Holes have unique faces.
--     Update with [LineSegment 2 Vertex r]
--     Update Face ids at the end.
--   Cut planar graph in two.
--   Re-triangulate part of graph.
--   Mesh smoothing.
--     1. Keep vertex positions separate. Can update without changing the graph.
--     2. Swap edges. HalfEdge+Twin. Find next of each. Delete original half-edges.
--        Then insert half-edges to next.

