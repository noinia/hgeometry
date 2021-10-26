{-# LANGUAGE RecordWildCards #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Data.PlanarGraph.Immutable
-- Copyright   :  (C) David Himmelstrup
-- License     :  see the LICENSE file
-- Maintainer  :  David Himmelstrup
--
-- A graph is planar if it can be drawn on a flat piece of
-- paper without any crossing edges. More theory is explained on wikipedia:
-- <https://en.wikipedia.org/wiki/Planar_graph>
--
-- This module describes the connectivity of planar graphs without knowing
-- the 2D coordinates of each vertex. Algorithms that require the vertex
-- positions (such as planar point locators or mesh smoothers) have to store
-- that information somewhere else. Vertices are identified by dense, consecutive
-- integers and can be efficiently used with arrays or finite maps.
-- 
-- A planar graph consists of directed edges (also called half-edges or darts),
-- vertices, and faces. If a face lies on the outside of a set of vertices, this
-- face is called a boundary.
--
-- The simplest planar graph has just three vertices and three edges:
--
-- @'pgFromFaces' [[0,1,2]]@
--
-- <<docs/Data/PlanarGraph/planargraph-2959979592048325618.svg>>
--
-- The above pictures shows three vertices (named @'0'@, @'1'@, and @'2'@), a single face
-- (named @'0'@ with an underscore), and 6 half-edges (named @'0'@ through @'5'@).
-- Vertices, faces, and half-edges can be efficiently queried and traversed.
--
-- >>> let pg = pgFromFaces [[0,1,2]]
-- >>> pgFaces pg
-- [Face 0]
-- >>> faceBoundary (Face 0) pg
-- [Vertex 1,Vertex 2,Vertex 0]
--
--
-- == Planar graph examples:
--
-- Faces in planar graphs do not have to be triangular:
--
-- @'pgFromFaces' [[0,1,2,3]]@
--
-- <<docs/Data/PlanarGraph/planargraph-2506803680640023584.svg>>
--
-- Vertices may be interior or lie on a boundary:
--
-- @'pgFromFaces' [[0,1,2,3],[4,3,2,1]]@
--
-- <<docs/Data/PlanarGraph/planargraph-1711135548958680232.svg>>
--
-- >>> let pg = pgFromFaces [[0,1,2,3],[4,3,2,1]]
-- >>> pgFaces pg
-- [Face 0,Face 1]
-- >>> vertexIsBoundary (Vertex 0) pg
-- True
-- >>> vertexIsBoundary (Vertex 2) pg
-- False
--
-- Planar graphs may have multiple boundaries. Notice how the area between vertices
-- @'1'@, @'2'@ and @'3'@ does not have a face ID:
--
-- @'pgFromFaces' [[0,4,1],[0,1,2],[4,3,1],[4,5,3],[3,5,2],[2,5,0]]@
--
-- <<docs/Data/PlanarGraph/planargraph-2635031442529484236.compact.svg>>
--
-- >>> let pg = pgFromFaces [[0,4,1],[0,1,2],[4,3,1],[4,5,3],[3,5,2],[2,5,0]]
-- >>> pgFaces pg
-- [Face 0,Face 1,Face 2,Face 3,Face 4,Face 5]
-- >>> pgBoundaries pg
-- [Boundary 0,Boundary 1]
-- >>> faceBoundary (Boundary 0) pg {- Outer boundary -}
-- [Vertex 0,Vertex 4,Vertex 5]
-- >>> faceBoundary (Boundary 1) pg {- Inner boundary -}
-- [Vertex 1,Vertex 2,Vertex 3]
--
-- Planar graphs may also have multiple unconnected components but they cannot be
-- automatically rendered:
--
-- >>> let pg = pgFromFaces [[0,1,2], [3,4,5]]
-- >>> pgFaces pg
-- [Face 0,Face 1]
-- >>> pgBoundaries pg
-- [Boundary 0,Boundary 1]
-- >>> faceBoundary (Boundary 0) pg
-- [Vertex 0,Vertex 1,Vertex 2]
-- >>> faceBoundary (Boundary 1) pg
-- [Vertex 3,Vertex 4,Vertex 5]
--
-- == Big-O Notation
--
-- When describing runtime complexity, @n@ refers to the size of the graph
-- (vertices, half-edges, faces, etcs). Some functions are output-sensitive
-- and use @k@ to indicate the amount of data consumed. For example,
-- 'vertexNeighbours' runs in \( O(k) \) and taking the first neighbour is therefore
-- an \( O(1) \) operation (because k=1).
--------------------------------------------------------------------------------
module Data.PlanarGraph.Immutable
  ( -- * Planar graphs
    PlanarGraph
  , pgFromFaces   -- :: [[VertexId]] -> PlanarGraph
  , pgFromFacesCV -- :: [CircularVector VertexId] -> PlanarGraph
  , pgVertices    -- :: PlanarGraph -> [Vertex]
  , pgEdges       -- :: PlanarGraph -> [Edge]
  , pgHalfEdges   -- :: PlanarGraph -> [HalfEdge]
  , pgFaces       -- :: PlanarGraph -> [Face]
  , pgBoundaries  -- :: PlanarGraph -> [Face]

    -- * Elements
    -- ** Vertices
  , Vertex(..)
  , vertexHalfEdge              -- :: Vertex -> PlanarGraph -> HalfEdge
  , vertexIsInterior            -- :: Vertex -> PlanarGraph -> Bool
  , vertexIsBoundary            -- :: Vertex -> PlanarGraph -> Bool
  , vertexOutgoingHalfEdges     -- :: Vertex -> PlanarGraph -> [HalfEdge]
  , vertexIncomingHalfEdges     -- :: Vertex -> PlanarGraph -> [HalfEdge]
  , vertexNeighbours            -- :: Vertex -> PlanarGraph -> [Vertex]

    -- ** Edges
  , Edge(..)
  , edgeHalfEdges        -- :: Edge -> (HalfEdge, HalfEdge)

    -- ** Half-edges
  , HalfEdge(..)
  , halfEdgeNext         -- :: HalfEdge -> PlanarGraph -> HalfEdge
  , halfEdgePrev         -- :: HalfEdge -> PlanarGraph -> HalfEdge
  , halfEdgeTwin         -- :: HalfEdge -> HalfEdge
  , halfEdgeNextOutgoing -- :: HalfEdge -> PlanarGraph -> HalfEdge
  , halfEdgeNextIncoming -- :: HalfEdge -> PlanarGraph -> HalfEdge
  , halfEdgeVertex       -- :: HalfEdge -> PlanarGraph -> Vertex
  , halfEdgeTailVertex   -- :: HalfEdge -> PlanarGraph -> Vertex
  , halfEdgeTipVertex    -- :: HalfEdge -> PlanarGraph -> Vertex
  , halfEdgeFace         -- :: HalfEdge -> PlanarGraph -> Face
  , halfEdgeIsInterior   -- :: HalfEdge -> PlanarGraph -> Bool
  , halfEdgeIsBoundary   -- :: HalfEdge -> PlanarGraph -> Bool

    -- ** Faces
  , Face(..), FaceId
  , faceMember     -- :: Face -> PlanarGraph -> Bool
  , faceId         -- :: Face -> FaceId
  , faceHalfEdge   -- :: Face -> PlanarGraph -> HalfEdge
  , faceIsInterior -- :: Face -> Bool
  , faceIsBoundary -- :: Face -> Bool
  , faceHalfEdges  -- :: Face -> PlanarGraph -> CircularVector HalfEdge
  , faceBoundary   -- :: Face -> PlanarGraph -> CircularVector Vertex

    -- * Mutation
  , pgMutate       -- :: PlanarGraph -> (forall s. Mut.PlanarGraph s -> ST s ()) -> PlanarGraph
  , pgCreate       -- :: (forall s. ST s (Mut.PlanarGraph s)) -> PlanarGraph
  , pgThaw         -- :: PlanarGraph -> ST s (Mut.PlanarGraph s)
  , pgFreeze       -- :: Mut.PlanarGraph s -> ST s PlanarGraph
  , pgUnsafeThaw   -- :: PlanarGraph -> ST s (Mut.PlanarGraph s)
  , pgUnsafeFreeze -- :: Mut.PlanarGraph s -> ST s PlanarGraph

    -- * Misc
  , tutteEmbedding -- :: PlanarGraph -> Vector.Vector (V2 Double)
  )
  where

import           Control.Monad
import           Control.Monad.ST
import           Data.Bits
import           Data.Coerce
import           Data.Hashable
import           Data.PlanarGraph.Internal (FaceId, HalfEdgeId, VertexId)
import qualified Data.PlanarGraph.Internal as Mut
import qualified Data.PlanarGraph.Mutable  as Mut
import           Data.Proxy
import           Data.STRef
import           Data.Vector               (Vector)
import qualified Data.Vector               as Vector
import           Data.Vector.Circular      (CircularVector)
import qualified Data.Vector.Circular      as CV
import qualified Data.Vector.Mutable       as V
import           GHC.Exts
import           Linear.Matrix             (luSolve)
import           Linear.V
import           Linear.V2

-- import Debug.Trace

-------------------------------------------------------------------------------
-- Elements: Half-edges, vertices, faces.


-- | Half-edges are directed edges between vertices. All Half-edge have a twin
--   in the opposite direction. Half-edges have individual identity but are always
--   created in pairs.
newtype HalfEdge = HalfEdge {halfEdgeId :: Int}
  deriving (Eq, Hashable)

instance Show HalfEdge where
  showsPrec d (HalfEdge v) = showParen (d > 10) $
    showString "HalfEdge " . shows v

instance Read HalfEdge where
  readsPrec d = readParen (d > app_prec) $ \r ->
      [ (HalfEdge v, t)
      | ("HalfEdge", s) <- lex r, (v, t) <- reads s ]
    where app_prec = 10

-- | Edges are bidirectional and connect two vertices. No two edges are allowed
-- to cross.
newtype Edge = Edge {edgeId :: Int}
  deriving (Eq, Hashable)

instance Show Edge where
  showsPrec d (Edge v) = showParen (d > 10) $
    showString "Edge " . shows v

instance Read Edge where
  readsPrec d = readParen (d > app_prec) $ \r ->
      [ (Edge v, t)
      | ("Edge", s) <- lex r, (v, t) <- reads s ]
    where app_prec = 10

-- | Graph vertices. For best performance, make sure to use consecutive numbers.
newtype Vertex = Vertex {vertexId :: Int}
  deriving (Eq, Hashable)

instance Show Vertex where
  showsPrec d (Vertex v) = showParen (d > 10) $
    showString "Vertex " . shows v

instance Read Vertex where
  readsPrec d = readParen (d > app_prec) $ \r ->
      [ (Vertex v, t)
      | ("Vertex", s) <- lex r, (v, t) <- reads s ]
    where app_prec = 10

-- | Faces are the areas divided by edges. If a face is not surrounded by a set of vertices,
--   it is called a boundary.
data Face = Face FaceId | Boundary FaceId
  deriving (Eq, Read, Show)

-------------------------------------------------------------------------------
-- Planar graph

-- PlanarGraphs have vertices, edges, and faces.
-- Invariant: The half-edge of a boundary vertex is interior, twin is exterior.

-- FIXME: Use STRefU ?
-- PlanarGraph with 0 vertices: No edges, no vertices, no faces.
-- PlanarGraph with 1 vertex: No edges, no interior faces.
-- PlanarGraph with 2 vertices: One edge, no interior faces.
-- PlanarGraph with 3+ vertices: Usual properties hold.
-- | Immutable planar graph.
data PlanarGraph = PlanarGraph
  { pgHalfEdgeNext   :: !(Vector HalfEdgeId) -- HalfEdge indexed
  , pgHalfEdgePrev   :: !(Vector HalfEdgeId) -- HalfEdge indexed
  , pgHalfEdgeVertex :: !(Vector VertexId)   -- HalfEdge indexed
  , pgHalfEdgeFace   :: !(Vector FaceId)     -- HalfEdge indexed
  , pgVertexEdges    :: !(Vector HalfEdgeId) -- Vertex indexed
  , pgFaceEdges      :: !(Vector HalfEdgeId) -- Face indexed
  , pgBoundaryEdges  :: !(Vector HalfEdgeId) -- Boundary faces
  } deriving Eq

panic :: String -> String -> a
panic tag msg = error $ "Data.PlanarGraph.Immutable." ++ tag ++ ": " ++ msg


-- Disabled since hashes are not stable across different versions of hashable.
-- // $setup
--
-- >>> hash $ pgFromFaces [[0,1,2]]
-- 2959979592048325618
-- >>> hash $ pgFromFaces [[1,2,3]]
-- 2486673127436488352
-- >>> hash $ pgFromFaces [[0,1,2,3]]
-- 2506803680640023584
-- >>> hash $ pgFromFaces [[0,1,2,3],[4,3,2,1]]
-- 1711135548958680232
--

-- | \( O(n \log n) \)
--
--   Construct a planar graph from a list of faces. Vertices are assumed to be dense
--   (ie without gaps) but this only affects performance, not correctness. Memory
--   usage is defined by the largest vertex ID. That means @'pgFromFaces' [[0,1,2]]@
--   has the same connectivity as @'pgFromFaces' [[7,8,9]]@ but uses three times less
--   memory.
--
-- ==== __Examples:__
-- @
-- 'pgFromFaces' [[0,1,2]]
-- @
-- <<docs/Data/PlanarGraph/planargraph-2959979592048325618.svg>>
--
-- @
-- 'pgFromFaces' [[0,1,2,3]]
-- @
-- <<docs/Data/PlanarGraph/planargraph-2506803680640023584.svg>>
--
-- @
-- 'pgFromFaces' [[1,2,3]]
-- @
-- <<docs/Data/PlanarGraph/planargraph-2486673127436488352.svg>>
--
-- @since 0.12.0.0
pgFromFaces :: [[VertexId]] -> PlanarGraph
pgFromFaces = pgFromFacesCV . map CV.unsafeFromList

-- | \( O(n \log n) \)
--
--   Construct a planar graph from a list of faces. This is a slightly more
--   efficient version of 'pgFromFacesCV'.
-- 
-- @since 0.12.0.0
pgFromFacesCV :: [CircularVector VertexId] -> PlanarGraph
pgFromFacesCV faces = pgCreate $ Mut.pgFromFacesCV faces

-- fromFaces' :: Int -> Int -> Int -> [CircularVector VertexId] -> ST s (PlanarGraph)
-- fromFaces' nFaces nHalfEdges maxVertexId faces = do
--   undefined

-- dualTree :: Face -> ST s (Tree Face)
-- dualTree = undefined

instance Hashable PlanarGraph where
  hashWithSalt salt = hashWithSalt salt . pgHash
  hash = pgHash

-- | O(n)
-- 
-- @since 0.12.0.0
pgHash :: PlanarGraph -> Int
pgHash pg =
  let loop [] salt = salt
      loop (edgeId:rest) salt =
        let he = HalfEdge (edgeId*2)
            vTail = halfEdgeTailVertex he pg
            vTip = halfEdgeTipVertex he pg
        in loop rest (hashWithSalt salt (vTail, vTip))
  in abs $ loop [0..Vector.length (pgHalfEdgeNext pg)`div`2-1] 0

-------------------------------------------------------------------------------
-- Vertices

-- | O(1)
vertexCheck :: String -> Vertex -> PlanarGraph -> a -> a
vertexCheck tag (Vertex vId) pg _val
  | vId >= Vector.length (pgVertexEdges pg) || vId < 0 =
    panic tag ("Out-of-bounds vertex access: " ++ show vId)
  | not (halfEdgeIsValid (HalfEdge (pgVertexEdges pg Vector.! vId))) =
    panic tag ("Tried to access deleted vertex: " ++ show vId)
vertexCheck _tag _face _pg val = val

-- | \( O(k) \)
--   
--   List all vertices in a graph.
--
-- ==== __Examples:__
--
-- >>> let pg = pgFromFaces [[0,1,2]]
--
-- <<docs/Data/PlanarGraph/planargraph-2959979592048325618.svg>>
--
-- >>> pgVertices pg
-- [Vertex 0,Vertex 1,Vertex 2]
--
-- @since 0.12.0.0
pgVertices :: PlanarGraph -> [Vertex]
pgVertices pg =
  [ Vertex v
  | v <- [0 .. Vector.length (pgVertexEdges pg)-1 ]
  , halfEdgeIsValid (HalfEdge $ pgVertexEdges pg Vector.! v)
  ]

-- -- | \( O(1) \)
-- vertexFromId :: VertexId -> Vertex
-- vertexFromId vId = Vertex vId

-- -- | \( O(1) \)
-- vertexId :: Vertex -> VertexId
-- vertexId (Vertex vId) = vId

-- Disabled since hashes are not stable across different versions of hashable.
-- // $hidden
--
-- >>> hash $ pgFromFaces [[0,1,2]]
-- 2959979592048325618

-- | \( O(1) \)
--
--   Each vertex has an assigned half-edge with the following properties:
--
--   @'halfEdgeVertex' ('vertexHalfEdge' vertex pg) pg = vertex@
--
--   @'faceIsInterior' ('halfEdgeFace' ('vertexHalfEdge' vertex pg) pg) = True@
--
-- ==== __Examples:__
-- >>> let pg = pgFromFaces [[0,1,2]]
--
-- <<docs/Data/PlanarGraph/planargraph-2959979592048325618.svg>>
--
-- >>> vertexHalfEdge (Vertex 0) pg
-- HalfEdge 4
--
-- >>> vertexHalfEdge (Vertex 1) pg
-- HalfEdge 0
--
-- >>> vertexHalfEdge (Vertex 6) pg
-- ... Exception: Data.PlanarGraph.Immutable.vertexHalfEdge: Out-of-bounds vertex access: 6
-- ...
--
-- >>> vertexHalfEdge (Vertex (-10)) pg
-- ... Exception: Data.PlanarGraph.Immutable.vertexHalfEdge: Out-of-bounds vertex access: -10
-- ...
--
-- >>> halfEdgeVertex (vertexHalfEdge (Vertex 2) pg) pg
-- Vertex 2
--
-- >>> halfEdgeFace (vertexHalfEdge (Vertex 0) pg) pg
-- Face 0
--
-- @since 0.12.0.0
vertexHalfEdge :: Vertex -> PlanarGraph -> HalfEdge
vertexHalfEdge vertex pg | vertexCheck "vertexHalfEdge" vertex pg False = undefined
vertexHalfEdge (Vertex vId) pg = HalfEdge $ pgVertexEdges pg Vector.! vId


-- Disabled since hashes are not stable across different versions of hashable.
-- // $hidden
--
-- >>> hash $ pgFromFaces [[0,1,2,3],[4,3,2,1]]
-- 1711135548958680232

-- | \( O(1) \)
--
--   Returns @True@ iff the vertex lies on a boundary.
--
-- ==== __Examples:__
-- >>> let pg = pgFromFaces [[0,1,2,3],[4,3,2,1]]
--
-- <<docs/Data/PlanarGraph/planargraph-1711135548958680232.svg>>
--
-- >>> vertexIsBoundary (Vertex 0) pg
-- True
--
-- >>> vertexIsBoundary (Vertex 2) pg
-- False
--
-- >>> vertexIsBoundary (Vertex 4) pg
-- True
--
-- >>> vertexIsBoundary (Vertex 12) pg
-- ... Exception: Data.PlanarGraph.Immutable.vertexIsBoundary: Out-of-bounds vertex access: 12
-- ...
--
-- @since 0.12.0.0
vertexIsBoundary :: Vertex -> PlanarGraph -> Bool
vertexIsBoundary vertex pg | vertexCheck "vertexIsBoundary" vertex pg False = undefined
vertexIsBoundary vertex pg =
    faceIsBoundary $ halfEdgeFace (halfEdgeTwin $ vertexHalfEdge vertex pg) pg

-- | \( O(1) \)
--
--   Returns @True@ iff the vertex is interior, ie. does not lie on a boundary.
--
-- ==== __Examples:__
-- >>> let pg = pgFromFaces [[0,1,2,3],[4,3,2,1]]
--
-- <<docs/Data/PlanarGraph/planargraph-1711135548958680232.svg>>
--
-- >>> vertexIsInterior (Vertex 0) pg
-- False
--
-- >>> vertexIsInterior (Vertex 2) pg
-- True
--
-- >>> vertexIsInterior (Vertex 4) pg
-- False
--
-- >>> vertexIsInterior (Vertex 12) pg
-- ... Exception: Data.PlanarGraph.Immutable.vertexIsInterior: Out-of-bounds vertex access: 12
-- ...
--
-- @since 0.12.0.0
vertexIsInterior :: Vertex -> PlanarGraph -> Bool
vertexIsInterior vertex pg | vertexCheck "vertexIsInterior" vertex pg False = undefined
vertexIsInterior vertex pg = not $ vertexIsBoundary vertex pg

-- | \( O(k) \)
--
--   Query outgoing half-edges from a given vertex in counter-clockwise order.
--
-- ==== __Examples:__
-- >>> let pg = pgFromFaces [[0,1,2,3],[4,3,2,1]]
--
-- <<docs/Data/PlanarGraph/planargraph-1711135548958680232.svg>>
--
-- >>> vertexOutgoingHalfEdges (Vertex 1) pg
-- [HalfEdge 0,HalfEdge 11,HalfEdge 3]
--
-- Each half-edge will point out from the origin vertex:
--
-- >>> map (`halfEdgeVertex` pg) $ vertexOutgoingHalfEdges (Vertex 1) pg
-- [Vertex 1,Vertex 1,Vertex 1]
--
-- >>> map (`halfEdgeTipVertex` pg) $ vertexOutgoingHalfEdges (Vertex 1) pg
-- [Vertex 0,Vertex 4,Vertex 2]
--
-- >>> vertexOutgoingHalfEdges (Vertex 2) pg
-- [HalfEdge 2,HalfEdge 5]
--
-- >>> vertexOutgoingHalfEdges (Vertex 12) pg
-- ... Exception: Data.PlanarGraph.Immutable.vertexOutgoingHalfEdges: Out-of-bounds vertex access: 12
-- ...
--
-- @since 0.12.0.0
vertexOutgoingHalfEdges :: Vertex -> PlanarGraph -> [HalfEdge]
vertexOutgoingHalfEdges vertex pg | vertexCheck "vertexOutgoingHalfEdges" vertex pg False = undefined
vertexOutgoingHalfEdges vertex pg = first : build (g (advance first))
  where
    advance he = halfEdgeNext (halfEdgeTwin he) pg
    first = vertexHalfEdge vertex pg
    g :: HalfEdge -> (HalfEdge -> b -> b) -> b -> b
    g he cons nil
      | he == first = nil
      | otherwise   = cons he (g (advance he) cons nil)

-- Disabled since hashes are not stable across different versions of hashable.
-- // $hidden
--
-- >>> hash $ pgFromFaces [[0,1,2,3],[4,3,2,1]]
-- 1711135548958680232

-- | \( O(k) \)
--
--   Query incoming half-edges from a given vertex in counter-clockwise order.
--
-- ==== __Examples:__
-- >>> let pg = pgFromFaces [[0,1,2,3],[4,3,2,1]]
--
-- <<docs/Data/PlanarGraph/planargraph-1711135548958680232.svg>>
--
-- >>> vertexIncomingHalfEdges (Vertex 1) pg
-- [HalfEdge 1,HalfEdge 10,HalfEdge 2]
--
-- >>> map (`halfEdgeVertex` pg) $ vertexIncomingHalfEdges (Vertex 1) pg
-- [Vertex 0,Vertex 4,Vertex 2]
--
-- >>> map (`halfEdgeTipVertex` pg) $ vertexIncomingHalfEdges (Vertex 1) pg
-- [Vertex 1,Vertex 1,Vertex 1]
--
-- >>> vertexIncomingHalfEdges (Vertex 2) pg
-- [HalfEdge 3,HalfEdge 4]
--
-- >>> vertexIncomingHalfEdges (Vertex 12) pg
-- ... Exception: Data.PlanarGraph.Immutable.vertexIncomingHalfEdges: Out-of-bounds vertex access: 12
-- ...
--
-- @since 0.12.0.0
vertexIncomingHalfEdges :: Vertex -> PlanarGraph -> [HalfEdge]
vertexIncomingHalfEdges vertex pg | vertexCheck "vertexIncomingHalfEdges" vertex pg False = undefined
vertexIncomingHalfEdges vertex pg = map halfEdgeTwin $ vertexOutgoingHalfEdges vertex pg

-- | \( O(k) \)
--
--   Query vertex neighbours in counter-clockwise order.
--
-- ==== __Examples:__
-- >>> let pg = pgFromFaces [[0,1,2,3],[4,3,2,1]]
--
-- <<docs/Data/PlanarGraph/planargraph-1711135548958680232.svg>>
--
-- >>> vertexNeighbours (Vertex 0) pg
-- [Vertex 3,Vertex 1]
--
-- >>> vertexNeighbours (Vertex 1) pg
-- [Vertex 0,Vertex 4,Vertex 2]
--
-- >>> vertexNeighbours (Vertex 2) pg
-- [Vertex 1,Vertex 3]
--
-- >>> vertexNeighbours (Vertex 12) pg
-- ... Exception: Data.PlanarGraph.Immutable.vertexNeighbours: Out-of-bounds vertex access: 12
-- ...
--
-- @since 0.12.0.0
vertexNeighbours :: Vertex -> PlanarGraph -> [Vertex]
vertexNeighbours vertex pg | vertexCheck "vertexNeighbours" vertex pg False = undefined
vertexNeighbours vertex pg = map (`halfEdgeVertex` pg) $ vertexIncomingHalfEdges vertex pg

-- vertexAdjacentVertices :: Vertex -> PlanarGraph -> [Vertex]
-- vertexAdjacentFaces :: Vertex -> PlanarGraph -> [Face]

-------------------------------------------------------------------------------
-- Edges

-- | \( O(k) \)
--   
--   List all edges in a graph.
--
-- ==== __Examples:__
--
-- >>> let pg = pgFromFaces [[0,1,2]]
--
-- <<docs/Data/PlanarGraph/planargraph-2959979592048325618.svg>>
--
-- >>> pgEdges pg
-- [Edge 0,Edge 1,Edge 2]
--
-- >>> map edgeHalfEdges $ pgEdges pg
-- [(HalfEdge 0,HalfEdge 1),(HalfEdge 2,HalfEdge 3),(HalfEdge 4,HalfEdge 5)]
--
-- @since 0.12.0.0
pgEdges :: PlanarGraph -> [Edge]
pgEdges pg =
  [ Edge e
  | e <- [0 .. Vector.length (pgHalfEdgeNext pg) `div` 2 -1]
  , let he = e*2
  , halfEdgeIsValid (HalfEdge he)
  ]

-- | \( O(1) \)
--
--   Split a bidirectional edge into directed half-edges.
--
-- @since 0.12.0.0
edgeHalfEdges :: Edge -> (HalfEdge, HalfEdge)
edgeHalfEdges (Edge e) = (HalfEdge $ e*2, HalfEdge $ e*2+1)

-------------------------------------------------------------------------------
-- Half-edges

-- | O(1)
halfEdgeCheck :: String -> HalfEdgeId -> PlanarGraph -> a -> a
halfEdgeCheck tag eId pg _val
  | eId >= Vector.length (pgHalfEdgeVertex pg) || eId < 0 =
    panic tag ("Out-of-bounds half-edge access: " ++ show eId)
  | pgHalfEdgeVertex pg Vector.! eId < 0 =
    panic tag ("Tried to access deleted half-edge: " ++ show eId)
halfEdgeCheck _tag _face _pg val = val

-- | \( O(k) \)
--   
--   List all half-edges in a graph.
--
-- ==== __Examples:__
--
-- >>> let pg = pgFromFaces [[0,1,2]]
--
-- <<docs/Data/PlanarGraph/planargraph-2959979592048325618.svg>>
--
-- >>> pgHalfEdges pg
-- [HalfEdge 0,HalfEdge 1,HalfEdge 2,HalfEdge 3,HalfEdge 4,HalfEdge 5]
--
-- @since 0.12.0.0
pgHalfEdges :: PlanarGraph -> [HalfEdge]
pgHalfEdges pg =
  [ he
  | he <- map HalfEdge [0 .. Vector.length (pgHalfEdgeNext pg)-1 ]
  , halfEdgeIsValid he
  ]


-- | O(1)
--
-- @since 0.12.0.0
halfEdgeIsValid :: HalfEdge -> Bool
halfEdgeIsValid (HalfEdge eId) = eId >= 0

-- -- | O(1)
-- halfEdgeFromId :: HalfEdgeId -> HalfEdge
-- halfEdgeFromId eId = HalfEdge eId

-- -- | O(1)
-- halfEdgeId :: HalfEdge -> HalfEdgeId
-- halfEdgeId (HalfEdge eId) = eId

-- Disabled since hashes are not stable across different versions of hashable.
-- // $hidden
--
-- >>> hash $ pgFromFaces [[0,1,2,3],[4,3,2,1]]
-- 1711135548958680232

-- | \( O(1) \)
--
--   Query the half-edge in the pointed direction. Internal half-edges
--   are arranged clockwise and external half-edges go counter-clockwise.
--
-- ==== __Examples:__
-- >>> let pg = pgFromFaces [[0,1,2,3],[4,3,2,1]]
--
-- <<docs/Data/PlanarGraph/planargraph-1711135548958680232.svg>>
--
-- >>> halfEdgeNext (HalfEdge 4) pg {- clockwise -}
-- HalfEdge 2
--
-- >>> halfEdgeNext (HalfEdge 3) pg {- clockwise -}
-- HalfEdge 5
--
-- >>> halfEdgeNext (HalfEdge 1) pg {- counter-clockwise -}
-- HalfEdge 11
--
-- >>> halfEdgeNext (HalfEdge 12) pg
-- ... Exception: Data.PlanarGraph.Immutable.halfEdgeNext: Out-of-bounds half-edge access: 12
-- ...
--
-- @since 0.12.0.0
halfEdgeNext :: HalfEdge -> PlanarGraph -> HalfEdge
halfEdgeNext (HalfEdge eId) pg = halfEdgeCheck "halfEdgeNext" eId pg $
  HalfEdge $ pgHalfEdgeNext pg Vector.! eId

-- | \( O(1) \)
--
--   Query the half-edge opposite the pointed direction. This means counter-clockwise
--   for internal half-edges and clockwise for external half-edges.
--
-- ==== __Examples:__
-- >>> let pg = pgFromFaces [[0,1,2,3],[4,3,2,1]]
--
-- <<docs/Data/PlanarGraph/planargraph-1711135548958680232.svg>>
--
-- >>> halfEdgePrev (HalfEdge 4) pg {- counter-clockwise -}
-- HalfEdge 6
--
-- >>> halfEdgePrev (HalfEdge 3) pg {- counter-clockwise -}
-- HalfEdge 10
--
-- >>> halfEdgePrev (HalfEdge 1) pg {- clockwise -}
-- HalfEdge 7
--
-- >>> halfEdgePrev (HalfEdge 12) pg
-- ... Exception: Data.PlanarGraph.Immutable.halfEdgePrev: Out-of-bounds half-edge access: 12
-- ...
--
-- @since 0.12.0.0
halfEdgePrev :: HalfEdge -> PlanarGraph -> HalfEdge
halfEdgePrev (HalfEdge eId) pg = halfEdgeCheck "halfEdgePrev" eId pg $
  HalfEdge $ pgHalfEdgePrev pg Vector.! eId

-- | \( O(1) \)
--
--   Next half-edge with the same vertex in counter-clockwise order.
--
-- ==== __Examples:__
-- >>> let pg = pgFromFaces [[0,1,2,3],[4,3,2,1]]
--
-- <<docs/Data/PlanarGraph/planargraph-1711135548958680232.svg>>
--
-- @HalfEdge 0@ is poiting out from @Vertex 1@. Moving counter-clockwise
-- around @Vertex 1@ yields @HalfEdge 11@ and @HalfEdge 3@.
--
-- >>> halfEdgeNextOutgoing (HalfEdge 0) pg
-- HalfEdge 11
--
-- >>> halfEdgeNextOutgoing (HalfEdge 11) pg
-- HalfEdge 3
--
-- >>> halfEdgeNextOutgoing (HalfEdge 3) pg
-- HalfEdge 0
--
-- >>> halfEdgeNextOutgoing (HalfEdge 12) pg
-- ... Exception: Data.PlanarGraph.Immutable.halfEdgeNextOutgoing: Out-of-bounds half-edge access: 12
-- ...
--
-- @since 0.12.0.0
halfEdgeNextOutgoing :: HalfEdge -> PlanarGraph -> HalfEdge
halfEdgeNextOutgoing e pg = halfEdgeCheck "halfEdgeNextOutgoing" (halfEdgeId e) pg $
  halfEdgeNext (halfEdgeTwin e) pg

-- | \( O(1) \)
--
--   Next half-edge with the same vertex in counter-clockwise order.
--
-- ==== __Examples:__
-- >>> let pg = pgFromFaces [[0,1,2,3],[4,3,2,1]]
--
-- <<docs/Data/PlanarGraph/planargraph-1711135548958680232.svg>>
--
-- @HalfEdge 6@ is poiting towards @Vertex 3@. Moving clockwise
-- around @Vertex 3@ yields @HalfEdge 11@ and @HalfEdge 3@.
--
-- >>> halfEdgeNextIncoming (HalfEdge 6) pg
-- HalfEdge 5
--
-- >>> halfEdgeNextIncoming (HalfEdge 5) pg
-- HalfEdge 9
--
-- >>> halfEdgeNextIncoming (HalfEdge 9) pg
-- HalfEdge 6
--
-- >>> halfEdgeNextIncoming (HalfEdge 12) pg
-- ... Exception: Data.PlanarGraph.Immutable.halfEdgeNextIncoming: Out-of-bounds half-edge access: 12
-- ...
--
-- @since 0.12.0.0
halfEdgeNextIncoming :: HalfEdge -> PlanarGraph -> HalfEdge
halfEdgeNextIncoming e pg = halfEdgeCheck "halfEdgeNextIncoming" (halfEdgeId e) pg $
  halfEdgeTwin (halfEdgeNext e pg)

-- | \( O(1) \)
--
-- @
-- 'halfEdgeTwin' . 'halfEdgeTwin' == id
-- @
--
-- @since 0.12.0.0
halfEdgeTwin :: HalfEdge -> HalfEdge
halfEdgeTwin (HalfEdge idx) = HalfEdge (idx `xor` 1)


-- | \( O(1) \)
--
--   Tail-end of a half-edge. Synonym of 'halfEdgeTailVertex'.
--
-- ==== __Examples:__
--
-- >>> let pg = pgFromFaces [[0,1,2]]
--
-- <<docs/Data/PlanarGraph/planargraph-2959979592048325618.svg>>
--
-- >>> halfEdgeVertex (HalfEdge 1) pg
-- Vertex 0
--
-- >>> halfEdgeVertex (HalfEdge 2) pg
-- Vertex 2
--
-- >>> halfEdgeVertex (HalfEdge 6) pg
-- ... Exception: Data.PlanarGraph.Immutable.halfEdgeVertex: Out-of-bounds half-edge access: 6
-- ...
--
-- @since 0.12.0.0
halfEdgeVertex :: HalfEdge -> PlanarGraph -> Vertex
halfEdgeVertex (HalfEdge idx) pg = halfEdgeCheck "halfEdgeVertex" idx pg $
  Vertex $ pgHalfEdgeVertex pg Vector.! idx

-- | O(1)
--
--   Tail-end of a half-edge. Synonym of 'halfEdgeVertex'.
--
-- ==== __Examples:__
--
-- >>> let pg = pgFromFaces [[0,1,2]]
--
-- <<docs/Data/PlanarGraph/planargraph-2959979592048325618.svg>>
--
-- >>> halfEdgeTailVertex (HalfEdge 1) pg
-- Vertex 0
--
-- >>> halfEdgeTailVertex (HalfEdge 2) pg
-- Vertex 2
--
-- @since 0.12.0.0
halfEdgeTailVertex :: HalfEdge -> PlanarGraph -> Vertex
halfEdgeTailVertex e pg = halfEdgeVertex e pg

-- | O(1)
--
--   Tip-end of a half-edge. This is the tail-end vertex of the twin half-edge.
--
-- ==== __Examples:__
--
-- >>> let pg = pgFromFaces [[0,1,2]]
--
-- <<docs/Data/PlanarGraph/planargraph-2959979592048325618.svg>>
--
-- >>> halfEdgeTipVertex (HalfEdge 1) pg
-- Vertex 1
--
-- >>> halfEdgeTipVertex (HalfEdge 5) pg
-- Vertex 0
--
-- @since 0.12.0.0
halfEdgeTipVertex  :: HalfEdge -> PlanarGraph -> Vertex
halfEdgeTipVertex e pg = halfEdgeVertex (halfEdgeTwin e) pg

-- Disabled since hashes are not stable across different versions of hashable.
-- // $hidden
--
-- >>> hash $ pgFromFaces [[0,1,2]]
-- 2959979592048325618

-- | \( O(1) \)
--
--   Query the face of a half-edge.
--
-- ==== __Examples:__
-- >>> let pg = pgFromFaces [[0,1,2]]
--
-- <<docs/Data/PlanarGraph/planargraph-2959979592048325618.svg>>
--
-- >>> halfEdgeFace (HalfEdge 0) pg
-- Face 0
--
-- >>> halfEdgeFace (HalfEdge 1) pg
-- Boundary 0
--
-- >>> halfEdgeFace (HalfEdge 6) pg
-- ... Exception: Data.PlanarGraph.Immutable.halfEdgeFace: Out-of-bounds half-edge access: 6
-- ...
--
-- @since 0.12.0.0
halfEdgeFace       :: HalfEdge -> PlanarGraph -> Face
halfEdgeFace (HalfEdge eId) pg = halfEdgeCheck "halfEdgeFace" eId pg $
  faceFromId $ pgHalfEdgeFace pg Vector.! eId

-- | \( O(1) \)
--
--   Check if a half-edge's face is on a boundary.
--
-- ==== __Examples:__
--
-- >>> let pg = pgFromFaces [[0,1,2]]
--
-- <<docs/Data/PlanarGraph/planargraph-2959979592048325618.svg>>
--
-- >>> halfEdgeIsBoundary (HalfEdge 0) pg
-- False
--
-- >>> halfEdgeIsBoundary (HalfEdge 1) pg
-- True
--
-- >>> halfEdgeIsBoundary (HalfEdge 2) pg
-- False
--
-- >>> halfEdgeIsBoundary (HalfEdge 3) pg
-- True
--
-- >>> halfEdgeIsBoundary (HalfEdge 6) pg
-- ... Exception: Data.PlanarGraph.Immutable.halfEdgeIsBoundary: Out-of-bounds half-edge access: 6
-- ...
--
-- @since 0.12.0.0
halfEdgeIsBoundary :: HalfEdge -> PlanarGraph -> Bool
halfEdgeIsBoundary edge pg = halfEdgeCheck "halfEdgeIsBoundary" (halfEdgeId edge) pg $
  faceIsBoundary $ halfEdgeFace edge pg

-- | \( O(1) \)
--
--   Check if a half-edge's face is interior.
--
-- ==== __Examples:__
--
-- >>> let pg = pgFromFaces [[0,1,2]]
--
-- <<docs/Data/PlanarGraph/planargraph-2959979592048325618.svg>>
--
-- >>> halfEdgeIsInterior (HalfEdge 0) pg
-- True
--
-- >>> halfEdgeIsInterior (HalfEdge 1) pg
-- False
--
-- >>> halfEdgeIsInterior (HalfEdge 2) pg
-- True
--
-- >>> halfEdgeIsInterior (HalfEdge 3) pg
-- False
--
-- >>> halfEdgeIsInterior (HalfEdge 6) pg
-- ... Exception: Data.PlanarGraph.Immutable.halfEdgeIsInterior: Out-of-bounds half-edge access: 6
-- ...
--
-- @since 0.12.0.0
halfEdgeIsInterior :: HalfEdge -> PlanarGraph -> Bool
halfEdgeIsInterior edge pg = halfEdgeCheck "halfEdgeIsInterior" (halfEdgeId edge) pg $
  faceIsInterior $ halfEdgeFace edge pg

-------------------------------------------------------------------------------
-- Faces

-- | \( O(k) \)
--   
--   List all faces in a graph.
--
-- ==== __Examples:__
--
-- >>> let pg = pgFromFaces [[0,4,1],[0,1,2],[4,3,1],[4,5,3],[3,5,2],[2,5,0]]
--
-- <<docs/Data/PlanarGraph/planargraph-2635031442529484236.compact.svg>>
--
-- >>> pgFaces pg
-- [Face 0,Face 1,Face 2,Face 3,Face 4,Face 5]
--
-- @since 0.12.0.0
pgFaces :: PlanarGraph -> [Face]
pgFaces pg =
  [ Face fId
  | fId <- [0 .. Vector.length (pgFaceEdges pg)-1 ]
  , halfEdgeIsValid (faceHalfEdge (Face fId) pg)
  ]

-- | \( O(k) \)
--   
--   List all boundaries (ie external faces) in a graph. There may be
--   multiple boundaries and they may or may not be reachable from each other.
--
-- ==== __Examples:__
--
-- >>> let pg = pgFromFaces [[0,4,1],[0,1,2],[4,3,1],[4,5,3],[3,5,2],[2,5,0]]
--
-- <<docs/Data/PlanarGraph/planargraph-2635031442529484236.compact.svg>>
--
-- >>> pgBoundaries pg
-- [Boundary 0,Boundary 1]
--
-- >>> faceBoundary (Boundary 0) pg
-- [Vertex 0,Vertex 4,Vertex 5]
--
-- >>> faceBoundary (Boundary 1) pg
-- [Vertex 1,Vertex 2,Vertex 3]
--
-- @since 0.12.0.0
pgBoundaries :: PlanarGraph -> [Face]
pgBoundaries pg =
  [ Boundary fId
  | fId <- [0 .. Vector.length (pgBoundaryEdges pg)-1 ]
  , halfEdgeIsValid (faceHalfEdge (Boundary fId) pg)
  ]

-- | O(1)
faceCheck :: String -> Face -> PlanarGraph -> a -> a
faceCheck tag (Face fId) pg _val
  | fId >= Vector.length (pgFaceEdges pg) || fId < 0 =
    panic tag ("Out-of-bounds face access: " ++ show fId)
  | pgFaceEdges pg Vector.! fId < 0 =
    panic tag ("Tried to access deleted face: " ++ show fId)
faceCheck tag (Boundary fId) pg _val
  | fId >= Vector.length (pgBoundaryEdges pg) || fId < 0 =
    panic tag ("Out-of-bounds boundary access: " ++ show fId)
  | pgBoundaryEdges pg Vector.! fId < 0 =
    panic tag ("Tried to access deleted boundary: " ++ show fId)
faceCheck _tag _face _pg val = val

-- | \( O(1) \)
--
--   Returns @True@ iff a face or boundary is part of the planar graph.
--
-- ==== __Examples:__
--
-- >>> let pg = pgFromFaces [[0,1,2]]
--
-- <<docs/Data/PlanarGraph/planargraph-2959979592048325618.svg>>
--
-- >>> faceMember (Face 0) pg
-- True
--
-- >>> faceMember (Face 1) pg
-- False
--
-- >>> faceMember (Face 100) pg
-- False
--
-- >>> faceMember (Face (-100)) pg
-- False
--
-- >>> faceMember (Boundary 0) pg
-- True
--
-- >>> faceMember (Boundary 1) pg
-- False
--
-- @since 0.12.0.0
faceMember :: Face -> PlanarGraph -> Bool
faceMember face@(Face fId) pg =
  fId < Vector.length (pgFaceEdges pg) &&
  fId >= 0 &&
  halfEdgeIsValid (faceHalfEdge face pg)
faceMember face@(Boundary fId) pg =
  fId < Vector.length (pgFaceEdges pg) &&
  fId >= 0 &&
  halfEdgeIsValid (faceHalfEdge face pg)


-- -- | O(1)
-- faceInvalid :: Face
-- faceInvalid = faceFromId maxBound

-- -- | O(1)
-- faceIsValid :: Face -> Bool
-- faceIsValid = not . faceIsInvalid

-- -- | O(1)
-- faceIsInvalid :: Face -> Bool
-- faceIsInvalid (Face fId)     = fId == maxBound
-- faceIsInvalid (Boundary fId) = fId == maxBound

-- | O(1)
--
-- @since 0.12.0.0
faceFromId :: FaceId -> Face
faceFromId fId | fId < 0 = Boundary (negate fId - 1)
faceFromId fId = Face fId

-- | \( O(1) \)
--
--   Maps interior faces to positive integers and boundary faces to negative integers.
--
-- ==== __Examples:__
--
-- >>> faceId (Face 0)
-- 0
--
-- >>> faceId (Face 10)
-- 10
--
-- >>> faceId (Boundary 0)
-- -1
--
-- >>> faceId (Boundary 10)
-- -11
--
-- @since 0.12.0.0
faceId :: Face -> FaceId
faceId (Face fId)     = fId
faceId (Boundary fId) = negate fId - 1


-- | \( O(1) \)
--
--   Query the half-edge associated with a face or boundary.
--
-- ==== __Examples:__
--
-- >>> let pg = pgFromFaces [[0,1,2,3],[4,3,2,1]]
--
-- <<docs/Data/PlanarGraph/planargraph-1711135548958680232.svg>>
--
-- >>> faceHalfEdge (Face 0) pg
-- HalfEdge 0
--
-- >>> faceHalfEdge (Face 1) pg
-- HalfEdge 8
--
-- >>> faceHalfEdge (Boundary 0) pg
-- HalfEdge 1
--
-- >>> faceHalfEdge (Face 10) pg {- Invalid face -}
-- ... Exception: Data.PlanarGraph.Immutable.faceHalfEdge: Out-of-bounds face access: 10
-- ...
--
-- @since 0.12.0.0
faceHalfEdge :: Face -> PlanarGraph -> HalfEdge
faceHalfEdge face pg = faceCheck "faceHalfEdge" face pg $
  case face of
    Face fId     -> HalfEdge $ pgFaceEdges pg Vector.! fId
    Boundary fId -> HalfEdge $ pgBoundaryEdges pg Vector.! fId

-- | \( O(1) \)
--
--   Returns @True@ iff a face is interior. Does not check if the face actually exists.
--
-- ==== __Examples:__
--
-- >>> faceIsInterior (Face 0)
-- True
--
-- >>> faceIsInterior (Face 10000)
-- True
--
-- >>> faceIsInterior (Boundary 0)
-- False
--
-- @since 0.12.0.0
faceIsInterior :: Face -> Bool
faceIsInterior = not . faceIsBoundary

-- | \( O(1) \)
--
--   Returns @True@ iff a face is a boundary. Does not check if the face actually exists.
--
-- ==== __Examples:__
--
-- >>> faceIsBoundary (Face 0)
-- False
--
-- >>> faceIsBoundary (Face 10000)
-- False
--
-- >>> faceIsBoundary (Boundary 0)
-- True
--
-- @since 0.12.0.0
faceIsBoundary :: Face -> Bool
faceIsBoundary Face{}     = False
faceIsBoundary Boundary{} = True

-- faceVertices         :: Face -> ST s (CircularVector Vertex)

-- | \( O(k) \)
--
--   Query the half-edges around a face in counter-clockwise order.
--
-- ==== __Examples:__
-- >>> let pg = pgFromFaces [[0,1,2,3],[4,3,2,1]]
--
-- <<docs/Data/PlanarGraph/planargraph-1711135548958680232.svg>>
--
-- >>> faceHalfEdges (Face 0) pg
-- [HalfEdge 0,HalfEdge 2,HalfEdge 4,HalfEdge 6]
--
-- >>> faceHalfEdges (Face 1) pg
-- [HalfEdge 8,HalfEdge 5,HalfEdge 3,HalfEdge 10]
--
-- >>> faceHalfEdges (Boundary 0) pg
-- [HalfEdge 1,HalfEdge 11,HalfEdge 9,HalfEdge 7]
--
-- >>> faceHalfEdges (Face 10) pg
-- ... Exception: Data.PlanarGraph.Immutable.faceHalfEdges: Out-of-bounds face access: 10
-- ...
--
-- @since 0.12.0.0
faceHalfEdges        :: Face -> PlanarGraph -> [HalfEdge]
faceHalfEdges face pg | faceCheck "faceHalfEdges" face pg False = undefined
faceHalfEdges face pg
  | faceIsBoundary face = first : build (worker halfEdgeNext (halfEdgeNext first pg))
  | otherwise           = first : build (worker halfEdgePrev (halfEdgePrev first pg))
  where
    first = faceHalfEdge face pg
    worker :: (HalfEdge -> PlanarGraph -> HalfEdge) -> HalfEdge -> (HalfEdge -> b -> b) -> b -> b
    worker advance he cons nil
      | he == first = nil
      | otherwise   = cons he (worker advance (advance he pg) cons nil)

-- | \( O(k) \)
--
--   Query the vertices of a face in counter-clockwise order.
--
-- ==== __Examples:__
-- >>> let pg = pgFromFaces [[0,1,2,3],[4,3,2,1]]
--
-- <<docs/Data/PlanarGraph/planargraph-1711135548958680232.svg>>
--
-- >>> faceBoundary (Face 0) pg
-- [Vertex 1,Vertex 2,Vertex 3,Vertex 0]
--
-- >>> faceBoundary (Face 1) pg
-- [Vertex 3,Vertex 2,Vertex 1,Vertex 4]
--
-- >>> faceBoundary (Boundary 0) pg
-- [Vertex 0,Vertex 1,Vertex 4,Vertex 3]
--
-- >>> faceBoundary (Face 10) pg
-- ... Exception: Data.PlanarGraph.Immutable.faceBoundary: Out-of-bounds face access: 10
-- ...
--
-- @since 0.12.0.0
faceBoundary :: Face -> PlanarGraph -> [Vertex]
faceBoundary face pg = faceCheck "faceBoundary" face pg $
  map (`halfEdgeVertex` pg) $ faceHalfEdges face pg

-- faceAdjacentFaces    :: Face -> ST s (CircularVector Face)

-------------------------------------------------------------------------------
-- Mutation

-- | \( O(n) \)
--
-- @since 0.12.0.0
pgMutate :: PlanarGraph -> (forall s. Mut.PlanarGraph s -> ST s ()) -> PlanarGraph
pgMutate pg action = runST $ do
  mutPG <- pgThaw pg
  action mutPG
  pgUnsafeFreeze mutPG

-- | \( O(1) \)
--
-- @since 0.12.0.0
pgCreate :: (forall s. ST s (Mut.PlanarGraph s)) -> PlanarGraph
pgCreate action = runST (action >>= pgUnsafeFreeze)

-- | \( O(n) \)
--
-- @since 0.12.0.0
pgThaw :: PlanarGraph -> ST s (Mut.PlanarGraph s)
pgThaw pg = do
  pgNextHalfEdgeId <- newSTRef $ Vector.length (pgHalfEdgeNext pg) `div` 2
  pgNextVertexId <- newSTRef $ Vector.length $ pgVertexEdges pg
  pgNextFaceId <- newSTRef $ Vector.length $ pgFaceEdges pg
  pgNextBoundaryId <- newSTRef $ Vector.length $ pgBoundaryEdges pg

  pgHalfEdgeNext   <- Mut.thawVector $ pgHalfEdgeNext pg
  pgHalfEdgePrev   <- Mut.thawVector $ pgHalfEdgePrev pg
  pgHalfEdgeFace   <- Mut.thawVector $ pgHalfEdgeFace pg
  pgHalfEdgeVertex <- Mut.thawVector $ pgHalfEdgeVertex pg
  pgVertexEdges <- Mut.thawVector $ pgVertexEdges pg
  pgFaceEdges <- Mut.thawVector $ pgFaceEdges pg
  pgBoundaryEdges <- Mut.thawVector $ pgBoundaryEdges pg
  pure Mut.PlanarGraph {..}

-- | \( O(1) \)
--
-- @since 0.12.0.0
pgUnsafeThaw :: PlanarGraph -> ST s (Mut.PlanarGraph s)
pgUnsafeThaw pg = do
  pgNextHalfEdgeId <- newSTRef $ Vector.length $ pgHalfEdgeNext pg
  pgNextVertexId <- newSTRef $ Vector.length $ pgVertexEdges pg
  pgNextFaceId <- newSTRef $ Vector.length $ pgFaceEdges pg
  pgNextBoundaryId <- newSTRef $ Vector.length $ pgBoundaryEdges pg

  pgHalfEdgeNext   <- Mut.unsafeThawVector $ pgHalfEdgeNext pg
  pgHalfEdgePrev   <- Mut.unsafeThawVector $ pgHalfEdgePrev pg
  pgHalfEdgeFace   <- Mut.unsafeThawVector $ pgHalfEdgeFace pg
  pgHalfEdgeVertex <- Mut.unsafeThawVector $ pgHalfEdgeVertex pg
  pgVertexEdges <- Mut.unsafeThawVector $ pgVertexEdges pg
  pgFaceEdges <- Mut.unsafeThawVector $ pgFaceEdges pg
  pgBoundaryEdges <- Mut.unsafeThawVector $ pgBoundaryEdges pg
  pure Mut.PlanarGraph {..}

-- | \( O(n) \)
--
-- @since 0.12.0.0
pgFreeze :: Mut.PlanarGraph s -> ST s PlanarGraph
pgFreeze pg = do
  maxEdgeId <- readSTRef (Mut.pgNextHalfEdgeId pg)
  maxVertexId <- readSTRef (Mut.pgNextVertexId pg)
  maxFaceId <- readSTRef (Mut.pgNextFaceId pg)
  maxBoundaryId <- readSTRef (Mut.pgNextBoundaryId pg)

  pgHalfEdgeNext   <- Vector.take (maxEdgeId*2) <$> Mut.freezeVector (Mut.pgHalfEdgeNext pg)
  pgHalfEdgePrev   <- Vector.take (maxEdgeId*2) <$> Mut.freezeVector (Mut.pgHalfEdgePrev pg)
  pgHalfEdgeFace   <- Vector.take (maxEdgeId*2) <$> Mut.freezeVector (Mut.pgHalfEdgeFace pg)
  pgHalfEdgeVertex <- Vector.take (maxEdgeId*2) <$> Mut.freezeVector (Mut.pgHalfEdgeVertex pg)
  pgVertexEdges <- Vector.take maxVertexId <$> Mut.freezeVector (Mut.pgVertexEdges pg)
  pgFaceEdges <- Vector.take maxFaceId <$> Mut.freezeVector (Mut.pgFaceEdges pg)
  pgBoundaryEdges <- Vector.take maxBoundaryId <$> Mut.freezeVector (Mut.pgBoundaryEdges pg)
  pure PlanarGraph { .. }

-- | \( O(1) \)
--
-- @since 0.12.0.0
pgUnsafeFreeze :: Mut.PlanarGraph s -> ST s PlanarGraph
pgUnsafeFreeze pg = do
  maxEdgeId <- readSTRef (Mut.pgNextHalfEdgeId pg)
  maxVertexId <- readSTRef (Mut.pgNextVertexId pg)
  maxFaceId <- readSTRef (Mut.pgNextFaceId pg)
  maxBoundaryId <- readSTRef (Mut.pgNextBoundaryId pg)

  pgHalfEdgeNext   <- Vector.take (maxEdgeId*2) <$> Mut.unsafeFreezeVector (Mut.pgHalfEdgeNext pg)
  pgHalfEdgePrev   <- Vector.take (maxEdgeId*2) <$> Mut.unsafeFreezeVector (Mut.pgHalfEdgePrev pg)
  pgHalfEdgeFace   <- Vector.take (maxEdgeId*2) <$> Mut.unsafeFreezeVector (Mut.pgHalfEdgeFace pg)
  pgHalfEdgeVertex <- Vector.take (maxEdgeId*2) <$> Mut.unsafeFreezeVector (Mut.pgHalfEdgeVertex pg)
  pgVertexEdges <- Vector.take maxVertexId <$> Mut.unsafeFreezeVector (Mut.pgVertexEdges pg)
  pgFaceEdges <- Vector.take maxFaceId <$> Mut.unsafeFreezeVector (Mut.pgFaceEdges pg)
  pgBoundaryEdges <- Vector.take maxBoundaryId <$> Mut.unsafeFreezeVector (Mut.pgBoundaryEdges pg)
  pure PlanarGraph { .. }

-------------------------------------------------------------------------------
-- Tutte embedding

-- | \( O(n^3) \)
--
-- @since 0.12.0.0
tutteEmbedding :: PlanarGraph -> Vector.Vector (V2 Double)
tutteEmbedding pg = runST $ do
  let nVertices = Vector.length (pgVertexEdges pg)
  -- trace ("nVertices: " ++ show nVertices) $ return ()
  m <- Vector.replicateM nVertices (V.replicate nVertices 0)
  vx <- V.replicate nVertices 0
  vy <- V.replicate nVertices 0

  let boundary = faceBoundary (Boundary 0) pg
  let nBoundary = length boundary
  -- trace ("Vectors: " ++ show boundary) $
  forM_ (zip boundary (regularPolygon nBoundary)) $ \(vertex,(x,y)) -> do
    let valid = halfEdgeIsValid $ vertexHalfEdge vertex pg
    when valid $ do
      V.write (m Vector.! vertexId vertex) (vertexId vertex) (1::Double)
      V.write vx (vertexId vertex) x
      V.write vy (vertexId vertex) y

  forM_ [0..nVertices-1] $ \vId -> -- trace ("Vertex: " ++ show vId) $
    do
      let valid = halfEdgeIsValid $ vertexHalfEdge (Vertex vId) pg
      unless valid $ do
        V.write (m Vector.! vId) vId (1::Double)
      when valid $ do
        let onOuterBoundary =
              Boundary 0 == halfEdgeFace (halfEdgeTwin $ vertexHalfEdge (Vertex vId) pg) pg
        unless onOuterBoundary $ do
          let vertex = Vertex vId
          let neighbours = vertexNeighbours vertex pg
          forM_ neighbours $ \neighbour ->
            V.write (m Vector.! vId) (vertexId neighbour) (1::Double)
          V.write (m Vector.! vId) vId (negate $ fromIntegral $ length neighbours)

  mi <- mapM Vector.freeze m
  vxi <- Vector.freeze vx
  vyi <- Vector.freeze vy

  let xPos = reifyMatrix mi vxi luSolve
      yPos = reifyMatrix mi vyi luSolve

  pure $ Vector.zipWith V2 xPos yPos

reifyMatrix :: forall a. Vector.Vector (Vector.Vector a) ->
  Vector.Vector a ->
  (forall (n :: *). Dim n => V n (V n a) -> V n a -> V n a) ->
  Vector.Vector a
reifyMatrix m v f = reifyDim (Vector.length m) $ \(Proxy :: Proxy n) ->
  toVector (f (coerce m :: (V n (V n a))) (coerce v))

regularPolygon :: Int -> [(Double, Double)]
regularPolygon n =
    [ (cos ang, sin ang)
    | i <- [0 .. n-1]
    , let ang = fromIntegral i * frac + pi/2]
  where
    frac = 2*pi / fromIntegral n
