{-# LANGUAGE RecordWildCards #-}
module Data.PlanarGraph.Immutable
  ( -- * Planar graphs
    PlanarGraph
  , pgFromFaces   -- :: [[VertexId]] -> PlanarGraph
  , pgFromFacesCV -- :: [CircularVector VertexId] -> PlanarGraph
  , pgHash        -- :: PlanarGraph -> Int
  , pgVertices    -- :: PlanarGraph -> [Vertex]
  , pgEdges       -- :: PlanarGraph -> [Edge]
  , pgHalfEdges   -- :: PlanarGraph -> [HalfEdge]
  , pgFaces       -- :: PlanarGraph -> [Face]

    -- * Elements
    -- ** Vertices
  , Vertex(..), VertexId
  , vertexFromId                -- :: VertexId -> PlanarGraph -> Vertex
  , vertexToId                  -- :: Vertex -> VertexId
  , vertexHalfEdge              -- :: Vertex -> PlanarGraph -> HalfEdge
  , vertexIsBoundary            -- :: Vertex -> PlanarGraph -> Bool
  , vertexOutgoingHalfEdges     -- :: Vertex -> PlanarGraph -> [HalfEdge]
  , vertexWithOutgoingHalfEdges -- :: Vertex -> PlanarGraph -> (HalfEdge -> ST s ()) -> ST s ()
  , vertexIncomingHalfEdges     -- :: Vertex -> PlanarGraph -> [HalfEdge]
  , vertexWithIncomingHalfEdges -- :: Vertex -> PlanarGraph -> (HalfEdge -> ST s ()) -> ST s ()
  , vertexNeighbours            -- :: Vertex -> PlanarGraph -> [Vertex]

    -- ** Edges
  , Edge(..)
  , edgeHalfEdges        -- :: Edge -> (HalfEdge, HalfEdge)

    -- ** Half-edges
  , HalfEdge(..), HalfEdgeId
  , halfEdgeFromId       -- :: HalfEdgeId -> PlanarGraph -> HalfEdge
  , halfEdgeToId         -- :: HalfEdge -> HalfEdgeId
  , halfEdgeNext         -- :: HalfEdge -> PlanarGraph -> HalfEdge
  , halfEdgePrev         -- :: HalfEdge -> PlanarGraph -> HalfEdge
  , halfEdgeNextOutgoing -- :: HalfEdge -> PlanarGraph -> HalfEdge
  , halfEdgeNextIncoming -- :: HalfEdge -> PlanarGraph -> HalfEdge
  , halfEdgeVertex       -- :: HalfEdge -> PlanarGraph -> Vertex
  , halfEdgeTwin         -- :: HalfEdge -> HalfEdge
  , halfEdgeTailVertex   -- :: HalfEdge -> PlanarGraph -> Vertex
  , halfEdgeTipVertex    -- :: HalfEdge -> PlanarGraph -> Vertex
  , halfEdgeFace         -- :: HalfEdge -> PlanarGraph -> Face
  , halfEdgeIsInterior   -- :: HalfEdge -> PlanarGraph -> Bool

    -- ** Faces
  , Face(..), FaceId
  , faceMember      -- :: Face -> PlanarGraph -> Bool
  , faceFromId     -- :: FaceId -> PlanarGraph -> Face
  , faceToId       -- :: Face -> FaceId
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
import           Data.PlanarGraph.Internal (FaceId, EdgeId, HalfEdgeId, VertexId)
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


-- | Half-edges are directed edges.
newtype HalfEdge = HalfEdge HalfEdgeId
  deriving (Eq, Show, Read, Hashable)


newtype Edge = Edge EdgeId
  deriving (Eq, Show, Read, Hashable)

newtype Vertex = Vertex VertexId
  deriving (Eq, Show, Read, Hashable)

data Face = Face FaceId | Boundary FaceId
  deriving (Eq, Show, Read)

-------------------------------------------------------------------------------
-- Planar graph

-- PlanarGraphs have vertices, edges, and faces.
-- Invariant: The half-edge of a boundary vertex is interior, twin is exterior.

-- FIXME: Use STRefU ?
-- PlanarGraph with 0 vertices: No edges, no vertices, no faces.
-- PlanarGraph with 1 vertex: No edges, no interior faces.
-- PlanarGraph with 2 vertices: One edge, no interior faces.
-- PlanarGraph with 3+ vertices: Usual properties hold.
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
panic tag msg = error $ "Data.PlanarGraph.Mutable." ++ tag ++ ": " ++ msg


-- $setup
--
-- >>> pgHash $ pgFromFaces [[0,1,2]]
-- 2959979592048325618
-- >>> pgHash $ pgFromFaces [[0,1,2,3]]
-- 2506803680640023584
-- >>> pgHash $ pgFromFaces [[0,1,2,3],[4,3,2,1]]
-- 1711135548958680232
--

-- | \( O(n \log n) \)
--
--
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
pgFromFaces :: [[VertexId]] -> PlanarGraph
pgFromFaces = pgFromFacesCV . map CV.unsafeFromList

pgFromFacesCV :: [CircularVector VertexId] -> PlanarGraph
pgFromFacesCV faces = pgCreate $ Mut.pgFromFacesCV faces

-- fromFaces' :: Int -> Int -> Int -> [CircularVector VertexId] -> ST s (PlanarGraph)
-- fromFaces' nFaces nHalfEdges maxVertexId faces = do
--   undefined

-- dualTree :: Face -> ST s (Tree Face)
-- dualTree = undefined

pgHash :: PlanarGraph -> Int
pgHash pg =
  let loop [] salt = salt
      loop (edgeId:rest) salt =
        let he = halfEdgeFromId (edgeId*2)
            vTail = halfEdgeTailVertex he pg
            vTip = halfEdgeTipVertex he pg
        in loop rest (hashWithSalt salt (vTail, vTip))
  in abs $ loop [0..Vector.length (pgHalfEdgeNext pg)`div`2-1] 0

-------------------------------------------------------------------------------
-- Vertices

-- | O(k)
pgVertices :: PlanarGraph -> [Vertex]
pgVertices pg =
  [ Vertex v
  | v <- [0 .. Vector.length (pgVertexEdges pg)-1 ]
  , halfEdgeIsValid (HalfEdge $ pgVertexEdges pg Vector.! v)
  ]

-- | \( O(1) \)
vertexFromId :: VertexId -> Vertex
vertexFromId vId = Vertex vId

-- | \( O(1) \)
vertexToId :: Vertex -> VertexId
vertexToId (Vertex vId) = vId


-- | \( O(1) \)
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
-- >>> halfEdgeVertex (vertexHalfEdge (Vertex 2) pg) pg
-- Vertex 2
--
-- >>> halfEdgeFace (vertexHalfEdge (Vertex 0) pg) pg
-- Face 0
vertexHalfEdge :: Vertex -> PlanarGraph -> HalfEdge
vertexHalfEdge (Vertex vId) pg = HalfEdge $ pgVertexEdges pg Vector.! vId

-- | \( O(1) \)
--   Returns @True@ iff the vertex lines on a boundary.
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
vertexIsBoundary :: Vertex -> PlanarGraph -> Bool
vertexIsBoundary vertex pg =
    faceIsBoundary $ halfEdgeFace (halfEdgeTwin $ vertexHalfEdge vertex pg) pg

-- | \( O(k) \)
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
vertexOutgoingHalfEdges :: Vertex -> PlanarGraph -> [HalfEdge]
vertexOutgoingHalfEdges vertex pg = first : build (g (advance first))
  where
    advance he = halfEdgeNext (halfEdgeTwin he) pg
    first = vertexHalfEdge vertex pg
    g :: HalfEdge -> (HalfEdge -> b -> b) -> b -> b
    g he cons nil
      | he == first = nil
      | otherwise   = cons he (g (advance he) cons nil)

-- | O(k)
vertexWithOutgoingHalfEdges :: Vertex -> PlanarGraph -> (HalfEdge -> ST s ()) -> ST s ()
vertexWithOutgoingHalfEdges vertex pg cb = do
  let first = vertexHalfEdge vertex pg
  cb first
  let loop edge | edge == first = return ()
      loop edge = do
        cb edge
        loop $ halfEdgeNext (halfEdgeTwin edge) pg
  loop $ halfEdgeNext (halfEdgeTwin first) pg

-- | \( O(k) \)
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
vertexIncomingHalfEdges :: Vertex -> PlanarGraph -> [HalfEdge]
vertexIncomingHalfEdges vertex pg = map halfEdgeTwin $ vertexOutgoingHalfEdges vertex pg

-- | O(k)
vertexWithIncomingHalfEdges :: Vertex -> (HalfEdge -> ST s ()) -> ST s ()
vertexWithIncomingHalfEdges = undefined

-- | \( O(k) \)
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
vertexNeighbours :: Vertex -> PlanarGraph -> [Vertex]
vertexNeighbours vertex pg = map (`halfEdgeVertex` pg) $ vertexIncomingHalfEdges vertex pg

-- vertexAdjacentVertices :: Vertex -> PlanarGraph -> [Vertex]
-- vertexAdjacentFaces :: Vertex -> PlanarGraph -> [Face]

-- O(1), internal function.
-- vertexNew :: PlanarGraph -> ST s Vertex
-- vertexNew pg = do
--   vId <- readSTRef (pgNextVertexId pg)
--   writeSTRef (pgNextVertexId pg) (vId+1)
--   return (Vertex vId pg)

-- vertexSetHalfEdge :: Vertex -> HalfEdge -> ST s ()
-- vertexSetHalfEdge (Vertex vId) (HalfEdge eId) = undefined

-------------------------------------------------------------------------------
-- Edges

-- | O(k)
pgEdges :: PlanarGraph -> [Edge]
pgEdges pg =
  [ Edge e
  | e <- [0 .. Vector.length (pgHalfEdgeNext pg) `div` 2 -1]
  , let he = e*2
  , halfEdgeIsValid (HalfEdge he)
  ]

-- | O(1)
edgeHalfEdges :: Edge -> (HalfEdge, HalfEdge)
edgeHalfEdges (Edge e) = (HalfEdge $ e*2, HalfEdge $ e*2+1)

-------------------------------------------------------------------------------
-- Half-edges

-- | O(k)
pgHalfEdges :: PlanarGraph -> [HalfEdge]
pgHalfEdges pg =
  [ he
  | he <- map HalfEdge [0 .. Vector.length (pgHalfEdgeNext pg)-1 ]
  , halfEdgeIsValid he
  ]


-- | O(1)
halfEdgeIsValid :: HalfEdge -> Bool
halfEdgeIsValid (HalfEdge eId) = eId >= 0

-- | O(1)
halfEdgeFromId :: HalfEdgeId -> HalfEdge
halfEdgeFromId eId = HalfEdge eId

-- | O(1)
halfEdgeToId :: HalfEdge -> HalfEdgeId
halfEdgeToId (HalfEdge eId) = eId


-- | \( O(1) \)
--
-- ==== __Examples:__
-- >>> let pg = pgFromFaces [[0,1,2,3],[4,3,2,1]]
--
-- <<docs/Data/PlanarGraph/planargraph-1711135548958680232.svg>>
--
-- >>> halfEdgeNext (HalfEdge 4) pg
-- HalfEdge 2
--
-- >>> halfEdgeNext (HalfEdge 3) pg
-- HalfEdge 5
halfEdgeNext :: HalfEdge -> PlanarGraph -> HalfEdge
halfEdgeNext (HalfEdge eId) pg = HalfEdge $ pgHalfEdgeNext pg Vector.! eId

-- | \( O(1) \)
--
-- ==== __Examples:__
-- >>> let pg = pgFromFaces [[0,1,2,3],[4,3,2,1]]
--
-- <<docs/Data/PlanarGraph/planargraph-1711135548958680232.svg>>
--
-- >>> halfEdgePrev (HalfEdge 4) pg
-- HalfEdge 6
--
-- >>> halfEdgePrev (HalfEdge 3) pg
-- HalfEdge 10
halfEdgePrev :: HalfEdge -> PlanarGraph -> HalfEdge
halfEdgePrev (HalfEdge eId) pg = HalfEdge $ pgHalfEdgePrev pg Vector.! eId

-- | O(1)
--   Next half-edge with the same vertex.
halfEdgeNextOutgoing :: HalfEdge -> PlanarGraph -> HalfEdge
halfEdgeNextOutgoing e pg = halfEdgeNext (halfEdgeTwin e) pg

-- | O(1)
--   Next half-edge with the same vertex.
halfEdgeNextIncoming :: HalfEdge -> PlanarGraph -> HalfEdge
halfEdgeNextIncoming e pg = halfEdgePrev (halfEdgeTwin e) pg

-- | O(1)
halfEdgeVertex     :: HalfEdge -> PlanarGraph -> Vertex
halfEdgeVertex (HalfEdge idx) pg = Vertex $ pgHalfEdgeVertex pg Vector.! idx

-- | O(1)
halfEdgeTwin       :: HalfEdge -> HalfEdge
halfEdgeTwin (HalfEdge idx) = HalfEdge (idx `xor` 1)

-- | O(1)
--   Synonym of `halfEdgeVertex`.
halfEdgeTailVertex :: HalfEdge -> PlanarGraph -> Vertex
halfEdgeTailVertex e pg = halfEdgeVertex e pg

-- | O(1)
--   Tip vertex. IE. the vertex of the twin edge.
halfEdgeTipVertex  :: HalfEdge -> PlanarGraph -> Vertex
halfEdgeTipVertex e pg = halfEdgeVertex (halfEdgeTwin e) pg

-- | \( O(1) \)
--
-- ==== __Examples:__
-- >>> let pg = pgFromFaces [[0,1,2]]
--
-- <<docs/Data/PlanarGraph/planargraph-2959979592048325618.svg>>
--
-- >>> halfEdgeFace (halfEdgeFromId 0) pg
-- Face 0
--
-- >>> halfEdgeFace (halfEdgeFromId 1) pg
-- Boundary 0
--
halfEdgeFace       :: HalfEdge -> PlanarGraph -> Face
halfEdgeFace (HalfEdge eId) pg = faceFromId $ pgHalfEdgeFace pg Vector.! eId

-- | \( O(1) \)
--   Check if a half-edge's face is interior or exterior.
--
-- ==== __Examples:__
-- @
-- 'pgFromFaces' [[0,1,2]]
-- @
--
-- <<docs/Data/PlanarGraph/planargraph-2959979592048325618.svg>>
--
-- >>> let pg = pgFromFaces [[0,1,2]]
-- >>> halfEdgeIsInterior (halfEdgeFromId 0) pg
-- True
--
-- >>> let pg = pgFromFaces [[0,1,2]]
-- >>> halfEdgeIsInterior (halfEdgeFromId 1) pg
-- False
--
-- >>> let pg = pgFromFaces [[0,1,2]]
-- >>> halfEdgeIsInterior (halfEdgeFromId 2) pg
-- True
--
-- >>> let pg = pgFromFaces [[0,1,2]]
-- >>> halfEdgeIsInterior (halfEdgeFromId 3) pg
-- False
halfEdgeIsInterior :: HalfEdge -> PlanarGraph -> Bool
halfEdgeIsInterior edge pg = faceIsInterior $ halfEdgeFace edge pg

-- O(1) Allocate new half-edge pair.
-- halfEdgeNew :: PlanarGraph -> ST s HalfEdge
-- halfEdgeNew pg = undefined
--   eId <- readSTRef (pgNextHalfEdgeId pg)
--   writeSTRef (pgNextHalfEdgeId pg) (eId+1)
--   return (HalfEdge (eId*2) pg)

-- halfEdgeSetNext :: HalfEdge -> HalfEdge -> PlanarGraph -> PlanarGraph
-- halfEdgeSetNext (HalfEdge e) (HalfEdge next) pg = undefined

-- halfEdgeSetPrev :: HalfEdge -> HalfEdge -> PlanarGraph -> PlanarGraph
-- halfEdgeSetPrev (HalfEdge e) (HalfEdge prev) pg = undefined

-- halfEdgeSetFace :: HalfEdge -> Face -> PlanarGraph -> PlanarGraph
-- halfEdgeSetFace (HalfEdge e) face pg =
--   pg{ pgHalfEdgeFace = pgHalfEdgeFace pg Vector.// [(e, faceToId face)] }

-- halfEdgeSetVertex :: HalfEdge -> Vertex -> PlanarGraph -> PlanarGraph
-- halfEdgeSetVertex (HalfEdge e) vertex pg =
--   pg{ pgHalfEdgeVertex = pgHalfEdgeVertex pg Vector.// [(e, vertexToId vertex)] }


-------------------------------------------------------------------------------
-- Faces

-- | O(k)
pgFaces :: PlanarGraph -> [Face]
pgFaces pg =
  [ Face fId
  | fId <- [0 .. Vector.length (pgFaceEdges pg)-1 ]
  , halfEdgeIsValid (faceHalfEdge (Face fId) pg)
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

-- | O(1)
faceMember :: Face -> PlanarGraph -> Bool
faceMember face@(Face fId) pg =
  (fId >= Vector.length (pgFaceEdges pg)) &&
  halfEdgeIsValid (faceHalfEdge face pg)
faceMember face@(Boundary fId) pg =
  (fId >= Vector.length (pgFaceEdges pg)) &&
  halfEdgeIsValid (faceHalfEdge face pg)


-- | O(1)
faceInvalid :: Face
faceInvalid = faceFromId maxBound

-- | O(1)
faceIsValid :: Face -> Bool
faceIsValid = not . faceIsInvalid

-- | O(1)
faceIsInvalid :: Face -> Bool
faceIsInvalid (Face fId)     = fId == maxBound
faceIsInvalid (Boundary fId) = fId == maxBound

-- | O(1)
faceFromId :: FaceId -> Face
faceFromId fId | fId < 0 = Boundary (negate fId - 1)
faceFromId fId = Face fId

-- | O(1)
faceToId :: Face -> FaceId
faceToId (Face fId)     = fId
faceToId (Boundary fId) = negate fId - 1


-- | O(1)
-- >>> let pg = pgFromFaces [[0,1,2]]
--
-- >>> faceHalfEdge (Face 0) pg
-- HalfEdge 0
--
-- >>> faceHalfEdge (Face 1) pg
-- ... Exception: Data.PlanarGraph.Mutable.faceHalfEdge: Out-of-bounds face access: 1
-- ...
--
faceHalfEdge :: Face -> PlanarGraph -> HalfEdge
faceHalfEdge face pg | faceCheck "faceHalfEdge" face pg False = undefined
faceHalfEdge (Face fId) pg     = HalfEdge $ pgFaceEdges pg Vector.! fId
faceHalfEdge (Boundary fId) pg = HalfEdge $ pgBoundaryEdges pg Vector.! fId

-- | O(1)
faceIsInterior :: Face -> Bool
faceIsInterior = not . faceIsBoundary

-- | O(1)
faceIsBoundary :: Face -> Bool
faceIsBoundary Face{}     = False
faceIsBoundary Boundary{} = True

-- faceVertices         :: Face -> ST s (CircularVector Vertex)

-- | O(k)
--   Counterclockwise vector of edges.
--
-- >>> let pg = pgFromFaces [[0,1,2]]
-- >>> faceHalfEdges (Face 0) pg
-- [HalfEdge 0,HalfEdge 2,HalfEdge 4]
faceHalfEdges        :: Face -> PlanarGraph -> [HalfEdge]
faceHalfEdges face pg
  | faceIsBoundary face = first : build (worker halfEdgeNext (halfEdgeNext first pg))
  | otherwise           = first : build (worker halfEdgePrev (halfEdgePrev first pg))
  where
    first = faceHalfEdge face pg
    worker :: (HalfEdge -> PlanarGraph -> HalfEdge) -> HalfEdge -> (HalfEdge -> b -> b) -> b -> b
    worker advance he cons nil
      | he == first = nil
      | otherwise   = cons he (worker advance (advance he pg) cons nil)

-- | O(k)
--
-- >>> let pg = pgFromFaces [[0,1,2]]
-- >>> faceBoundary (Face 0) pg
-- [Vertex 1,Vertex 2,Vertex 0]
faceBoundary :: Face -> PlanarGraph -> [Vertex]
faceBoundary face pg = map (`halfEdgeVertex` pg) $ faceHalfEdges face pg

-- faceAdjacentFaces    :: Face -> ST s (CircularVector Face)

-------------------------------------------------------------------------------
-- Mutation

-- | O(n)
pgMutate :: PlanarGraph -> (forall s. Mut.PlanarGraph s -> ST s ()) -> PlanarGraph
pgMutate pg action = runST $ do
  mutPG <- pgThaw pg
  action mutPG
  pgUnsafeFreeze mutPG

-- | O(1)
pgCreate :: (forall s. ST s (Mut.PlanarGraph s)) -> PlanarGraph
pgCreate action = runST (action >>= pgUnsafeFreeze)

-- | O(n)
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

-- | O(1)
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

-- | O(n)
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

-- | O(1)
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

-- { pgNextHalfEdgeId :: !(STRef s HalfEdgeId)
--   , pgNextVertexId   :: !(STRef s VertexId)
--   , pgNextFaceId     :: !(STRef s FaceId)
--   , pgNextBoundaryId :: !(STRef s FaceId)
--   , pgHalfEdgeNext   :: !(GrowVector s HalfEdgeId) -- HalfEdge indexed
--   , pgHalfEdgePrev   :: !(GrowVector s HalfEdgeId) -- HalfEdge indexed
--   , pgHalfEdgeVertex :: !(GrowVector s VertexId)   -- HalfEdge indexed
--   , pgHalfEdgeFace   :: !(GrowVector s FaceId)     -- HalfEdge indexed
--   , pgVertices       :: !(GrowVector s HalfEdgeId) -- Vertex indexed
--   , pgFaceEdges          :: !(GrowVector s HalfEdgeId) -- Face indexed
--   , pgBoundaryEdges     :: !(GrowVector s HalfEdgeId) -- Boundary faces

-- pgConnectVertices :: HalfEdge -> HalfEdge -> ST s (Edge s)
-- pgConnectVertices e1 e2 = do
--   -- Check e1.face == e2.face
--   -- Check e1.next /= e2
--   -- Check e2.next /= e1
--   -- create new half-edge pair: e and e'
--   -- e.vertex = e1.vertex
--   -- e.next = e2
--   --
--   undefined

-- pgSplitHalfEdge :: HalfEdge -> ST s Vertex

-- pgRemoveFace :: Face -> ST s ()
-- pgRemoveHalfEdge :: HalfEdge -> ST s ()
-- pgRemoveVertex :: Vertex -> ST s ()

-------------------------------------------------------------------------------
-- Use cases

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

-------------------------------------------------------------------------------
-- Tutte embedding

-- | \( O(n^3) \)
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
      V.write (m Vector.! vertexToId vertex) (vertexToId vertex) (1::Double)
      V.write vx (vertexToId vertex) x
      V.write vy (vertexToId vertex) y

  forM_ [0..nVertices-1] $ \vId -> -- trace ("Vertex: " ++ show vId) $
    do
      let valid = halfEdgeIsValid $ vertexHalfEdge (vertexFromId vId) pg
      unless valid $ do
        V.write (m Vector.! vId) vId (1::Double)
      when valid $ do
        let onBoundary = vertexIsBoundary (vertexFromId vId) pg
        unless onBoundary $ do
          let vertex = vertexFromId vId
          let neighbours = vertexNeighbours vertex pg
          forM_ neighbours $ \neighbour ->
            V.write (m Vector.! vId) (vertexToId neighbour) (1::Double)
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

