module Data.PlanarGraph.Persistent where

import Data.HashMap.Strict

-- | The world in which the graph lives
data World = Primal | Dual deriving (Show,Eq)

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