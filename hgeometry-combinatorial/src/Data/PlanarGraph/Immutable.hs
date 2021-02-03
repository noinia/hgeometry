{-# LANGUAGE RecordWildCards #-}
module Data.PlanarGraph.Immutable
  ( -- * Planar graphs
    PlanarGraph(..)
  , pgFromFaces   -- :: [[VertexId]] -> ST s (PlanarGraph)
  , pgFromFacesCV -- :: [CircularVector VertexId] -> ST s (PlanarGraph)
  , pgClone       -- :: PlanarGraph -> ST s (PlanarGraph)
  , pgHash        -- :: PlanarGraph -> ST s Int

    -- * Elements
    -- ** Vertices
  , Vertex, VertexId
  , vertexFromId                -- :: VertexId -> PlanarGraph -> Vertex
  , vertexToId                  -- :: Vertex -> VertexId
  , vertexHalfEdge              -- :: Vertex -> ST s HalfEdge
  , vertexIsBoundary            -- :: Vertex -> ST s Bool
  , vertexOutgoingHalfEdges     -- :: Vertex -> ST s (CircularVector HalfEdge)
  , vertexWithOutgoingHalfEdges -- :: Vertex -> (HalfEdge -> ST s ()) -> ST s ()
  , vertexIncomingHalfEdges     -- :: Vertex -> ST s (CircularVector HalfEdge)
  , vertexWithIncomingHalfEdges -- :: Vertex -> (HalfEdge -> ST s ()) -> ST s ()
  , vertexNeighbours            -- :: Vertex -> ST s (CircularVector Vertex)
  -- , vertexNew -- :: PlanarGraph -> ST s Vertex
  -- , vertexSetHalfEdge -- :: Vertex -> HalfEdge -> ST s ()

    -- ** Half-edges
  , HalfEdge, HalfEdgeId
  , halfEdgeFromId       -- :: HalfEdgeId -> PlanarGraph -> HalfEdge
  , halfEdgeToId         -- :: HalfEdge -> HalfEdgeId
  , halfEdgeNext         -- :: HalfEdge -> ST s HalfEdge
  , halfEdgePrev         -- :: HalfEdge -> ST s HalfEdge
  , halfEdgeNextOutgoing -- :: HalfEdge -> ST s HalfEdge
  , halfEdgeNextIncoming -- :: HalfEdge -> ST s HalfEdge
  , halfEdgeVertex       -- :: HalfEdge -> ST s Vertex
  , halfEdgeTwin         -- :: HalfEdge -> HalfEdge
  , halfEdgeTailVertex   -- :: HalfEdge -> ST s Vertex
  , halfEdgeTipVertex    -- :: HalfEdge -> ST s Vertex
  , halfEdgeFace         -- :: HalfEdge -> ST s (Face s)
  , halfEdgeIsInterior   -- :: HalfEdge -> ST s Bool
  -- , halfEdgeNew          -- :: PlanarGraph -> ST s HalfEdge
  -- , halfEdgeSetNext      -- :: HalfEdge -> HalfEdge -> ST s ()
  -- , halfEdgeSetPrev      -- :: HalfEdge -> HalfEdge -> ST s ()
  -- , halfEdgeSetFace      -- :: HalfEdge -> Face s -> ST s ()
  -- , halfEdgeSetVertex    -- :: HalfEdge -> Vertex -> ST s ()

    -- ** Faces
  , Face, FaceId
  , faceInvalid    -- :: PlanarGraph -> Face s
  , faceIsValid    -- :: Face s -> Bool
  , faceIsInvalid  -- :: Face s -> Bool
  , faceFromId     -- :: FaceId -> PlanarGraph -> Face s
  , faceToId       -- :: Face s -> FaceId
  , faceHalfEdge   -- :: Face s -> ST s HalfEdge
  , faceIsInterior -- :: Face s -> Bool
  , faceIsBoundary -- :: Face s -> Bool
  , faceHalfEdges  -- :: Face s -> ST s (CircularVector HalfEdge)
  , faceBoundary   -- :: Face s -> ST s (CircularVector Vertex)
  -- , faceNew :: PlanarGraph -> ST s (Face s)
  -- , faceNewBoundary :: PlanarGraph -> ST s (Face s)
  -- , faceSetHalfEdge :: Face s -> HalfEdge -> ST s ()

    -- * Mutation
  , pgMutate
  , pgCreate
  , pgThaw
  , pgFreeze
  , pgUnsafeThaw
  , pgUnsafeFreeze
  -- , pgConnectVertices -- :: HalfEdge -> HalfEdge -> ST s (Edge s)
-- pgSplitHalfEdge :: HalfEdge -> ST s Vertex

-- pgRemoveFace :: Face s -> ST s ()
-- pgRemoveHalfEdge :: HalfEdge -> ST s ()
-- pgRemoveVertex :: Vertex -> ST s ()
    -- * Misc
  , tutteEmbedding -- :: PlanarGraph -> Vector.Vector (V2 Double)
  , freezeCircularVector
  )
  where

import           Control.Monad
import           Control.Monad.ST
import           Data.Bits
import           Data.Hashable
import           Data.PlanarGraph.Internal (freezeCircularVector, newVector, writeVector )
import qualified Data.PlanarGraph.Internal as Mut
import qualified Data.PlanarGraph.Mutable  as Mut
import           Data.STRef
import           Data.Vector.Circular      (CircularVector)
import qualified Data.Vector.Circular      as CV
import qualified Data.Vector.Mutable       as V

import           Data.Coerce
import           Data.Proxy
import           Data.Vector   (Vector)
import qualified Data.Vector   as Vector
import           Linear.Matrix (luSolve)
import           Linear.V
import           Linear.V2

import Debug.Trace

-------------------------------------------------------------------------------
-- Elements: Half-edges, vertices, faces.



type HalfEdgeId = Int
newtype HalfEdge = HalfEdge HalfEdgeId
  deriving (Eq)
instance Show HalfEdge where
  showsPrec d (HalfEdge s) = showsPrec d s
instance Hashable HalfEdge where
  hashWithSalt salt (HalfEdge eId) = hashWithSalt salt eId


newtype Edge s = Edge Int
  deriving Eq

type VertexId = Int
newtype Vertex = Vertex VertexId
  deriving Eq
instance Show Vertex where
  showsPrec d (Vertex v) = showsPrec d v
instance Hashable Vertex where
  hashWithSalt salt (Vertex vId) = hashWithSalt salt vId

type FaceId = Int
data Face = Face FaceId | Boundary FaceId
  deriving Eq
instance Show Face where
  showsPrec d (Face fId)     = showString "Face " . shows fId
  showsPrec d (Boundary fId) = showString "Boundary " . shows fId

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
  , pgVertices       :: !(Vector HalfEdgeId) -- Vertex indexed
  , pgFaces          :: !(Vector HalfEdgeId) -- Face indexed
  , pgBoundaries     :: !(Vector HalfEdgeId) -- Boundary faces
  } deriving Eq

-- panic :: String -> String -> a
-- panic tag msg = error $ "Data.PlanarGraph.Mutable." ++ tag ++ ": " ++ msg


-- $setup
--
-- >>> pgHash $ pgFromFaces [[0,1,2]]
-- 2753226191199495654
-- >>> pgHash $ pgFromFaces [[0,1,2,3]]
-- 2271197297257670264

-- | \( O(n \log n) \)
--
--
--
-- ==== __Examples:__
-- @
-- 'pgFromFaces' [[0,1,2]]
-- @
-- <<docs/Data/PlanarGraph/planargraph-2753226191199495654.svg>>
--
-- @
-- 'pgFromFaces' [[0,1,2,3]]
-- @
-- <<docs/Data/PlanarGraph/planargraph-2271197297257670264.svg>>
pgFromFaces :: [[VertexId]] -> PlanarGraph
pgFromFaces = pgFromFacesCV . map CV.unsafeFromList

pgFromFacesCV :: [CircularVector VertexId] -> PlanarGraph
pgFromFacesCV faces = pgCreate $ Mut.pgFromFacesCV faces

-- fromFaces' :: Int -> Int -> Int -> [CircularVector VertexId] -> ST s (PlanarGraph)
-- fromFaces' nFaces nHalfEdges maxVertexId faces = do
--   undefined

-- | \( O(n) \)
pgClone :: PlanarGraph -> ST s (PlanarGraph)
pgClone = undefined

-- dualTree :: Face s -> ST s (Tree (Face s))
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

-- | \( O(1) \)
vertexFromId :: VertexId -> Vertex
vertexFromId vId = Vertex vId

-- | \( O(1) \)
vertexToId :: Vertex -> VertexId
vertexToId (Vertex vId) = vId

-- | \( O(1) \)
vertexHalfEdge :: Vertex -> PlanarGraph -> HalfEdge
vertexHalfEdge (Vertex vId) pg = HalfEdge $ pgVertices pg Vector.! vId

-- | \( O(1) \)
vertexIsBoundary :: Vertex -> PlanarGraph -> Bool
vertexIsBoundary vertex pg =
    faceIsBoundary $ halfEdgeFace (halfEdgeTwin $ vertexHalfEdge vertex pg) pg

-- -- | O(k)
vertexOutgoingHalfEdges :: Vertex -> PlanarGraph -> CircularVector HalfEdge
vertexOutgoingHalfEdges vertex pg = runST $ do
  tmp <- newVector 10
  iRef <- newSTRef 0
  vertexWithOutgoingHalfEdges vertex pg $ \edge -> do
    i <- readSTRef iRef
    modifySTRef' iRef succ
    writeVector tmp i edge
  i <- readSTRef iRef
  freezeCircularVector i tmp

-- -- | O(k), more efficient than 'vertexOutgoingHalfEdges'.
vertexWithOutgoingHalfEdges :: Vertex -> PlanarGraph -> (HalfEdge -> ST s ()) -> ST s ()
vertexWithOutgoingHalfEdges vertex pg cb = do
  let first = vertexHalfEdge vertex pg
  cb first
  let loop edge | edge == first = return ()
      loop edge = trace ("At edge: "++ show (first, edge)) $ do
        cb edge
        loop $ halfEdgeNext (halfEdgeTwin edge) pg
  loop $ halfEdgeNext (halfEdgeTwin first) pg

-- | O(k)
vertexIncomingHalfEdges :: Vertex -> PlanarGraph -> CircularVector HalfEdge
vertexIncomingHalfEdges vertex pg = CV.map halfEdgeTwin $ vertexOutgoingHalfEdges vertex pg

-- | O(k)
vertexWithIncomingHalfEdges :: Vertex -> (HalfEdge -> ST s ()) -> ST s ()
vertexWithIncomingHalfEdges = undefined

-- | O(k)
vertexNeighbours :: Vertex -> PlanarGraph -> CircularVector Vertex
vertexNeighbours vertex pg = CV.map (`halfEdgeVertex` pg) $ vertexIncomingHalfEdges vertex pg

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
-- Half-edges

-- | O(1)
halfEdgeIsValid :: HalfEdge -> Bool
halfEdgeIsValid (HalfEdge eId) = eId >= 0

-- | O(1)
halfEdgeFromId :: HalfEdgeId -> HalfEdge
halfEdgeFromId eId = HalfEdge eId

-- | O(1)
halfEdgeToId :: HalfEdge -> HalfEdgeId
halfEdgeToId (HalfEdge eId) = eId

-- | O(1)
halfEdgeNext :: HalfEdge -> PlanarGraph -> HalfEdge
halfEdgeNext (HalfEdge eId) pg = HalfEdge $ pgHalfEdgeNext pg Vector.! eId

-- | O(1)
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
--   Tail vertex. IE. the vertex of the twin edge.
halfEdgeTailVertex :: HalfEdge -> PlanarGraph -> Vertex
halfEdgeTailVertex e pg = halfEdgeVertex (halfEdgeTwin e) pg

-- | O(1)
--   Synonym of `halfEdgeVertex`.
halfEdgeTipVertex  :: HalfEdge -> PlanarGraph -> Vertex
halfEdgeTipVertex = halfEdgeVertex

-- | \( O(1) \)
--
-- ==== __Examples:__
-- @
-- 'pgFromFaces' [[0,1,2]]
-- @
--
-- <<docs/Data/PlanarGraph/planargraph-2753226191199495654.svg>>
--
-- >>> let pg = pgFromFaces [[0,1,2]]
-- >>> halfEdgeFace (halfEdgeFromId 0) pg
-- Face 0
--
-- >>> let pg = pgFromFaces [[0,1,2]]
-- >>> halfEdgeFace (halfEdgeFromId 1) pg
-- Boundary 0
--
halfEdgeFace       :: HalfEdge -> PlanarGraph -> Face
halfEdgeFace (HalfEdge eId) pg = faceFromId $ pgHalfEdgeFace pg Vector.! eId

-- -- | O(n)
-- --   Scan boundary half-edges without using 'next' or 'prev'.
-- halfEdgeConstructBoundary :: HalfEdge -> ST s (CircularVector HalfEdge)
-- halfEdgeConstructBoundary halfEdge = {- trace ("mkBoundary from: " ++ show halfEdge) $ -} do
--   tmp <- newVector 10
--   writeVector tmp 0 halfEdge
--   let loop i edge | edge == halfEdge = {- trace "Done" $ -} return i
--       loop i edge = {- trace ("Going to: " ++ show edge) $ -} do
--         face <- halfEdgeFace edge
--         if faceIsInvalid face
--           then do
--             writeVector tmp i edge
--             loop (i+1) =<< (halfEdgeTwin <$> halfEdgeNextIncoming edge)
--           else
--             loop i =<< (halfEdgeTwin <$> halfEdgePrev edge)
--   i <- loop 1 =<< (halfEdgeTwin <$> halfEdgeNextIncoming halfEdge)
--   cv <- freezeCircularVector i tmp
--   -- trace ("Boundary: " ++ show cv) $ pure cv
--   pure cv

-- $setup
-- >>> let genPG = pgFromFaces [[0,1,2]]

-- | \( O(1) \)
--   Check if a half-edge's face is interior or exterior.
--
-- ==== __Examples:__
-- @
-- 'pgFromFaces' [[0,1,2]]
-- @
--
-- <<docs/Data/PlanarGraph/planargraph-2753226191199495654.svg>>
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
faceHalfEdge         :: Face -> PlanarGraph -> HalfEdge
faceHalfEdge (Face fId) pg =
  let eId = pgFaces pg Vector.! fId
  in HalfEdge eId
faceHalfEdge (Boundary fId) pg =
  let eId = pgBoundaries pg Vector.! fId
  in HalfEdge eId

-- | O(1)
faceIsInterior       :: Face -> Bool
faceIsInterior = not . faceIsBoundary

-- | O(1)
faceIsBoundary       :: Face -> Bool
faceIsBoundary Face{}     = False
faceIsBoundary Boundary{} = True

-- faceVertices         :: Face s -> ST s (CircularVector Vertex)

-- | O(k)
--   Counterclockwise vector of edges.
faceHalfEdges        :: Face -> PlanarGraph -> CircularVector HalfEdge
faceHalfEdges face pg
  | faceIsBoundary face = worker halfEdgeNext
  | otherwise           = worker halfEdgePrev
  where
    worker advance = runST $ do
      let first = faceHalfEdge face pg
      tmp <- newVector 10
      writeVector tmp 0 first
      let loop i edge | edge == first = return i
          loop i edge = do
            writeVector tmp i edge
            loop (i+1) (advance edge pg)
      i <- loop 1 (advance first pg)
      freezeCircularVector i tmp

-- | O(k)
faceBoundary :: Face -> PlanarGraph -> CircularVector Vertex
faceBoundary face pg = CV.map (`halfEdgeVertex` pg) $ faceHalfEdges face pg

-- faceAdjacentFaces    :: Face s -> ST s (CircularVector (Face s))

-- faceNew :: PlanarGraph -> ST s (Face s)
-- faceNew pg = do
--   fId <- readSTRef (pgNextFaceId pg)
--   writeSTRef (pgNextFaceId pg) (fId+1)
--   return (Face fId pg)

-- faceNewBoundary :: PlanarGraph -> ST s (Face s)
-- faceNewBoundary pg = do
--   fId <- readSTRef (pgNextBoundaryId pg)
--   writeSTRef (pgNextBoundaryId pg) (fId+1)
--   return (Boundary fId pg)

-- faceSetHalfEdge :: Face -> HalfEdge -> PlanarGraph -> PlanarGraph
-- faceSetHalfEdge = undefined
-- faceSetHalfEdge (Boundary fId pg) (HalfEdge eId pg') = eqCheck "faceSetHalfEdge" pg pg' $
--   -- trace ("faceSetHalfEdge: " ++ show (fId, eId)) $
--   writeVector (pgBoundaries pg) fId eId
-- faceSetHalfEdge (Face fId pg) (HalfEdge eId pg') = eqCheck "faceSetHalfEdge" pg pg' $
--   writeVector (pgFaces pg) fId eId

-------------------------------------------------------------------------------
-- Mutation

-- | O(n)
pgMutate :: PlanarGraph -> (forall s. Mut.PlanarGraph s -> ST s ()) -> PlanarGraph
pgMutate pg action = runST $ do
  mutPG <- pgThaw pg
  action mutPG
  pgUnsafeFreeze mutPG

pgCreate :: (forall s. ST s (Mut.PlanarGraph s)) -> PlanarGraph
pgCreate action = runST (action >>= pgUnsafeFreeze)

-- | O(n)
pgThaw :: PlanarGraph -> ST s (Mut.PlanarGraph s)
pgThaw pg = do
  pgNextHalfEdgeId <- newSTRef $ Vector.length $ pgHalfEdgeNext pg
  pgNextVertexId <- newSTRef $ Vector.length $ pgVertices pg
  pgNextFaceId <- newSTRef $ Vector.length $ pgFaces pg
  pgNextBoundaryId <- newSTRef $ Vector.length $ pgBoundaries pg

  pgHalfEdgeNext   <- Mut.thawVector $ pgHalfEdgeNext pg
  pgHalfEdgePrev   <- Mut.thawVector $ pgHalfEdgePrev pg
  pgHalfEdgeFace   <- Mut.thawVector $ pgHalfEdgeFace pg
  pgHalfEdgeVertex <- Mut.thawVector $ pgHalfEdgeVertex pg
  pgVertices <- Mut.thawVector $ pgVertices pg
  pgFaces <- Mut.thawVector $ pgFaces pg
  pgBoundaries <- Mut.thawVector $ pgBoundaries pg
  pure Mut.PlanarGraph {..}

-- | O(1)
pgUnsafeThaw :: PlanarGraph -> ST s (Mut.PlanarGraph s)
pgUnsafeThaw pg = do
  pgNextHalfEdgeId <- newSTRef $ Vector.length $ pgHalfEdgeNext pg
  pgNextVertexId <- newSTRef $ Vector.length $ pgVertices pg
  pgNextFaceId <- newSTRef $ Vector.length $ pgFaces pg
  pgNextBoundaryId <- newSTRef $ Vector.length $ pgBoundaries pg

  pgHalfEdgeNext   <- Mut.unsafeThawVector $ pgHalfEdgeNext pg
  pgHalfEdgePrev   <- Mut.unsafeThawVector $ pgHalfEdgePrev pg
  pgHalfEdgeFace   <- Mut.unsafeThawVector $ pgHalfEdgeFace pg
  pgHalfEdgeVertex <- Mut.unsafeThawVector $ pgHalfEdgeVertex pg
  pgVertices <- Mut.unsafeThawVector $ pgVertices pg
  pgFaces <- Mut.unsafeThawVector $ pgFaces pg
  pgBoundaries <- Mut.unsafeThawVector $ pgBoundaries pg
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
  pgVertices <- Vector.take maxVertexId <$> Mut.freezeVector (Mut.pgVertices pg)
  pgFaces <- Vector.take maxFaceId <$> Mut.freezeVector (Mut.pgFaces pg)
  pgBoundaries <- Vector.take maxBoundaryId <$> Mut.freezeVector (Mut.pgBoundaries pg)
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
  pgVertices <- Vector.take maxVertexId <$> Mut.unsafeFreezeVector (Mut.pgVertices pg)
  pgFaces <- Vector.take maxFaceId <$> Mut.unsafeFreezeVector (Mut.pgFaces pg)
  pgBoundaries <- Vector.take maxBoundaryId <$> Mut.unsafeFreezeVector (Mut.pgBoundaries pg)
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
--   , pgFaces          :: !(GrowVector s HalfEdgeId) -- Face indexed
--   , pgBoundaries     :: !(GrowVector s HalfEdgeId) -- Boundary faces

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

-- pgRemoveFace :: Face s -> ST s ()
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
  let nVertices = Vector.length (pgVertices pg)
  -- trace ("nVertices: " ++ show nVertices) $ return ()
  m <- Vector.replicateM nVertices (V.replicate nVertices 0)
  vx <- V.replicate nVertices 0
  vy <- V.replicate nVertices 0

  let boundary = faceBoundary (Boundary 0) pg
  let nBoundary = length boundary
  -- trace ("Vectors: " ++ show boundary) $
  CV.forM_ (CV.zip boundary (regularPolygon nBoundary)) $ \(vertex,(x,y)) -> do
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
          CV.forM_ neighbours $ \neighbour ->
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

regularPolygon :: Int -> CircularVector (Double, Double)
regularPolygon n = CV.unsafeFromList
    [ (cos ang, sin ang)
    | i <- [0 .. n-1]
    , let ang = fromIntegral i * frac + pi/2]
  where
    frac = 2*pi / fromIntegral n

