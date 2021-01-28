module Data.PlanarGraph.Mutable
  ( PlanarGraph
  )
  where

import           Control.Monad
import           Control.Monad.ST
import           Data.Bits
import qualified Data.HashMap.Strict  as HM
import           Data.STRef
import qualified Data.Vector          as V (unsafeFreeze)
import           Data.Vector.Circular (CircularVector)
import qualified Data.Vector.Circular as CV
import           Data.Vector.Mutable  (STVector)
import qualified Data.Vector.Mutable  as V

import           Data.Coerce
import           Data.Proxy
import qualified Data.Vector   as Vector
import           Linear.Matrix (luSolve)
import           Linear.V

import Debug.Trace

-- Only works in docs? Yes.
-- 
-- >>> runPG $ do pg <- fromFaces []; vertexDelete (vertexFromId 0 pg); pure pg
-- SomeHash
--
-- ![image description](pathtoimage.png)
test = undefined

-------------------------------------------------------------------------------
-- Resizeable vector

type GrowVector s v = STRef s (STVector s v)

newVector :: Int -> ST s (GrowVector s v)
newVector n = newSTRef =<< V.new n

readVector :: GrowVector s v -> Int -> ST s v
readVector ref idx = do
  v <- readSTRef ref
  V.read v idx

writeVector :: GrowVector s v -> Int -> v -> ST s ()
writeVector ref idx val = do
  v <- readSTRef ref
  let l = V.length v
  if idx >= l
    then trace ("Growing: " ++ show (idx, l)) $ do
      v' <- V.grow v ((idx+1)*2)
      V.write v' idx val
      writeSTRef ref v'
    else -- trace ("Writing: " ++ show (idx, l)) $
      V.write v idx val

freezeCircularVector :: Int -> GrowVector s v -> ST s (CircularVector v)
freezeCircularVector n ref =
  (CV.unsafeFromVector . Vector.take n) <$> (V.unsafeFreeze =<< readSTRef ref)


-------------------------------------------------------------------------------
-- Elements: Half-edges, vertices, faces.



type HalfEdgeId = Int
data HalfEdge s = HalfEdge HalfEdgeId (PlanarGraph s)
  deriving Eq
instance Show (HalfEdge s) where
  showsPrec d (HalfEdge s _) = showsPrec d s

data Edge s = Edge Int (PlanarGraph s)
  deriving Eq

type VertexId = Int
data Vertex s = Vertex VertexId (PlanarGraph s)
  deriving Eq
instance Show (Vertex s) where
  showsPrec d (Vertex v _) = showsPrec d v

type FaceId = Int
data Face s = Face FaceId (PlanarGraph s) | Boundary FaceId (PlanarGraph s)
  deriving Eq
instance Show (Face s) where
  showsPrec d = showsPrec d . faceToId

-------------------------------------------------------------------------------
-- Planar graph

-- PlanarGraphs have vertices, edges, and faces.
-- Invariant: The half-edge of a boundary vertex is interior, twin is exterior.

-- FIXME: Use STRefU ?
-- PlanarGraph with 0 vertices: No edges, no vertices, no faces.
-- PlanarGraph with 1 vertex: No edges, no interior faces.
-- PlanarGraph with 2 vertices: One edge, no interior faces.
-- PlanarGraph with 3+ vertices: Usual properties hold.
data PlanarGraph s = PlanarGraph
  { pgNextHalfEdgeId :: !(STRef s HalfEdgeId)
  , pgNextVertexId   :: !(STRef s VertexId)
  , pgNextFaceId     :: !(STRef s FaceId)
  , pgNextBoundaryId :: !(STRef s FaceId)
  , pgHalfEdgeNext   :: !(GrowVector s HalfEdgeId) -- HalfEdge indexed
  , pgHalfEdgePrev   :: !(GrowVector s HalfEdgeId) -- HalfEdge indexed
  , pgHalfEdgeVertex :: !(GrowVector s VertexId)   -- HalfEdge indexed
  , pgHalfEdgeFace   :: !(GrowVector s FaceId)     -- HalfEdge indexed
  , pgVertices       :: !(GrowVector s HalfEdgeId) -- Vertex indexed
  , pgFaces          :: !(GrowVector s HalfEdgeId) -- Face indexed
  , pgBoundaries     :: !(GrowVector s HalfEdgeId) -- Boundary faces
  } deriving Eq

panic :: String -> String -> a
panic tag msg = error $ "Data.PlanarGraph.Mutable." ++ tag ++ ": " ++ msg

eqCheck :: String -> PlanarGraph s -> PlanarGraph s -> a -> a
eqCheck tag pg1 pg2 v
  | pg1 == pg2 = v
  | otherwise = panic tag "Invalid cross reference."

empty :: Int -> Int -> Int -> ST s (PlanarGraph s)
empty nFaces nVertices nEdges = PlanarGraph
  <$> newSTRef 0
  <*> newSTRef 0
  <*> newSTRef 0
  <*> newSTRef 0
  <*> newVector (nEdges*2)
  <*> newVector (nEdges*2)
  <*> newVector (nEdges*2)
  <*> newVector (nEdges*2)
  <*> newVector nVertices
  <*> newVector nFaces
  <*> newVector 0

{-
  For all boundary vertices:
    vertex.half-edge.face == interior
    vertex.half-edge.twin.face == exterior
  Boundary face: 0

  create N
-}
-- | O(n)
--   Create a planar graph with N boundary vertices.
new :: Int -> ST s (PlanarGraph s)
new n | n < 0 = panic "new" "Cannot contain negative vertices."
new 0 = empty 0 0 0
new 1 = undefined
new 2 = undefined
new n = fromFaces [CV.unsafeFromList [0..n-1]]

{-

-}
-- | O(n log n)
fromFaces :: [CircularVector VertexId] -> ST s (PlanarGraph s)
fromFaces [] = empty 0 0 0
fromFaces faces = do
  let maxVertexId = maximum (map CV.maximum faces)
      nFaces = length faces
      nHalfEdges = sum (map length faces)
  pg <- empty nFaces (maxVertexId+1) (nHalfEdges `div` 2)
  writeSTRef (pgNextVertexId pg) (maxVertexId+1)
  edgeMap <- newSTRef HM.empty
  let getHalfEdge (vTail, vTip) = do
        hm <- readSTRef edgeMap
        case HM.lookup (vTail, vTip) hm of
          Just{} -> panic "fromFaces" "Duplicate half-edge."
          Nothing ->
            case HM.lookup (vTip, vTail) hm of
              Just twin -> pure (halfEdgeTwin twin)
              Nothing   -> trace ("Creating new half-edge: " ++ show (vTip, vTail)) $ do
                halfEdge <- halfEdgeNew pg
                halfEdgeSetFace (halfEdgeTwin halfEdge) (faceInvalid pg)
                vertexSetHalfEdge (vertexFromId vTail pg) halfEdge
                writeSTRef edgeMap $ HM.insert (vTail, vTip) halfEdge hm
                halfEdgeSetVertex halfEdge (vertexFromId vTail pg)
                halfEdgeSetVertex (halfEdgeTwin halfEdge) (vertexFromId vTip pg)
                pure halfEdge
      addFace face | length face < 3 = panic "fromFaces" "Faces must have at least 3 vertices."
      addFace face = trace "Adding face" $ do
        fId <- faceNew pg
        let edges = CV.zip face (CV.rotateRight 1 face)
        halfEdges <- trace ("getHalfEdge") $ mapM getHalfEdge edges
        faceSetHalfEdge fId (CV.head halfEdges)
        setNextPrevFace fId halfEdges
  forM_ faces addFace

  -- For each half-edge:
  --   If face is invalid:
  --     Find loop and add it as a boundary.
  forM_ (map (`halfEdgeFromId` pg) [0..nHalfEdges-1]) $ \he -> trace ("Scan halfedge: " ++ show he) $ do
    f <- halfEdgeFace he
    validFace <- faceIsValid <$> halfEdgeFace he
    unless validFace $ trace ("Found invalid face: " ++ show f) $ do
      face <- faceNewBoundary pg
      boundary <- trace "mkBoundary" $ halfEdgeConstructBoundary he
      trace ("Boundary: " ++ show boundary) $ return ()
      trace "setEdge" $ faceSetHalfEdge face (CV.head boundary)
      trace "setEdge done" $ setNextPrevFace face boundary
  pure pg
  where
    setNextPrevFace fId halfEdges = do
      let edgeTriples = CV.zip3 (CV.rotateLeft 1 halfEdges) halfEdges (CV.rotateRight 1 halfEdges)
      forM_ (edgeTriples) $ \(prev, edge, next) -> trace ("setNextPrevFace: " ++ show (prev,edge,next)) $ do
          halfEdgeSetNext edge next
          halfEdgeSetPrev edge prev
          halfEdgeSetFace edge fId

-- fromFaces' :: Int -> Int -> Int -> [CircularVector VertexId] -> ST s (PlanarGraph s)
-- fromFaces' nFaces nHalfEdges maxVertexId faces = do
--   undefined

-- | O(n)
clone :: PlanarGraph s -> ST s (PlanarGraph s)
clone = undefined

-- dualTree :: Face s -> ST s (Tree (Face s))
-- dualTree = undefined

-------------------------------------------------------------------------------
-- Vertices

-- | O(1)
vertexFromId :: VertexId -> PlanarGraph s -> Vertex s
vertexFromId vId pg = Vertex vId pg

-- | O(1)
vertexToId :: Vertex s -> VertexId
vertexToId (Vertex vId _pg) = vId

-- | O(1)
vertexHalfEdge :: Vertex s -> ST s (HalfEdge s)
vertexHalfEdge (Vertex vId pg) = do
  eId <- readVector (pgVertices pg) vId
  pure $ HalfEdge eId pg

-- | O(1)
vertexIsBoundary :: Vertex s -> ST s Bool
vertexIsBoundary vertex = faceIsBoundary <$> (halfEdgeFace =<< (halfEdgeTwin <$> vertexHalfEdge vertex))

-- | O(k)
vertexOutgoingHalfEdges :: Vertex s -> ST s (CircularVector (HalfEdge s))
vertexOutgoingHalfEdges vertex = do
  tmp <- newVector 10
  iRef <- newSTRef 0
  vertexWithOutgoingHalfEdges vertex $ \edge -> do
    i <- readSTRef iRef
    modifySTRef' iRef succ
    writeVector tmp i edge
  i <- readSTRef iRef
  freezeCircularVector i tmp

-- | O(k), more efficient than 'vertexOutgoingHalfEdges'.
vertexWithOutgoingHalfEdges :: Vertex s -> (HalfEdge s -> ST s ()) -> ST s ()
vertexWithOutgoingHalfEdges vertex cb = do
  first <- vertexHalfEdge vertex
  cb first
  let loop edge | edge == first = return ()
      loop edge = trace ("At edge: "++ show (first, edge)) $ do
        cb edge
        loop =<< halfEdgeNext (halfEdgeTwin edge)
  loop =<< halfEdgeNext (halfEdgeTwin first)

-- | O(k)
vertexIncomingHalfEdges :: Vertex s -> ST s (CircularVector (HalfEdge s))
vertexIncomingHalfEdges vertex = CV.map halfEdgeTwin <$> vertexOutgoingHalfEdges vertex

-- | O(k)
vertexWithIncomingHalfEdges :: Vertex s -> (HalfEdge s -> ST s ()) -> ST s ()
vertexWithIncomingHalfEdges = undefined

-- | O(k)
vertexNeighbours :: Vertex s -> ST s (CircularVector (Vertex s))
vertexNeighbours vertex = CV.mapM halfEdgeVertex =<< vertexIncomingHalfEdges vertex

-- vertexAdjacentVertices :: Vertex -> PlanarGraph -> [Vertex]
-- vertexAdjacentFaces :: Vertex -> PlanarGraph -> [Face]

-- O(1), internal function.
vertexNew :: PlanarGraph s -> ST s (Vertex s)
vertexNew pg = do
  vId <- readSTRef (pgNextVertexId pg)
  writeSTRef (pgNextVertexId pg) (vId+1)
  return (Vertex vId pg)

vertexSetHalfEdge :: Vertex s -> HalfEdge s -> ST s ()
vertexSetHalfEdge (Vertex vId pg) (HalfEdge eId pg') = eqCheck "vertexSetHalfEdge" pg pg' $
  writeVector (pgVertices pg) vId eId

-------------------------------------------------------------------------------
-- Half-edges

-- | O(1)
halfEdgeFromId :: HalfEdgeId -> PlanarGraph s -> HalfEdge s
halfEdgeFromId eId pg = HalfEdge eId pg

-- | O(1)
halfEdgeToId :: HalfEdge s -> HalfEdgeId
halfEdgeToId (HalfEdge eId _pg) = eId

-- | O(1)
halfEdgeNext :: HalfEdge s -> ST s (HalfEdge s)
halfEdgeNext (HalfEdge eId pg) = do
  next <- readVector (pgHalfEdgeNext pg) eId
  pure $ HalfEdge next pg

-- | O(1)
halfEdgePrev :: HalfEdge s -> ST s (HalfEdge s)
halfEdgePrev (HalfEdge eId pg) = do
  prev <- readVector (pgHalfEdgePrev pg) eId
  pure $ HalfEdge prev pg

-- | O(1)
--   Next half-edge with the same vertex.
halfEdgeNextOutgoing :: HalfEdge s -> ST s (HalfEdge s)
halfEdgeNextOutgoing = halfEdgeNext . halfEdgeTwin

-- | O(1)
--   Next half-edge with the same vertex.
halfEdgeNextIncoming :: HalfEdge s -> ST s (HalfEdge s)
halfEdgeNextIncoming = halfEdgePrev . halfEdgeTwin

-- | O(1)
halfEdgeVertex     :: HalfEdge s -> ST s (Vertex s)
halfEdgeVertex (HalfEdge idx pg) = do
  v <- readVector (pgHalfEdgeVertex pg) idx
  pure $ Vertex v pg

-- | O(1)
halfEdgeTwin       :: HalfEdge s -> HalfEdge s
halfEdgeTwin (HalfEdge idx graph) = HalfEdge (idx `xor` 1) graph

-- | O(1)
--   Tail vertex. IE. the vertex of the twin edge.
halfEdgeTailVertex :: HalfEdge s -> ST s (Vertex s)
halfEdgeTailVertex = halfEdgeVertex . halfEdgeTwin

-- | O(1)
--   Synonym of `halfEdgeVertex`.
halfEdgeTipVertex  :: HalfEdge s -> ST s (Vertex s)
halfEdgeTipVertex = halfEdgeVertex

-- | O(1)
halfEdgeFace       :: HalfEdge s -> ST s (Face s)
halfEdgeFace (HalfEdge eId pg) = do
  fId <- readVector (pgHalfEdgeFace pg) eId
  pure $ faceFromId fId pg

-- | O(n)
--   Scan boundary half-edges without using 'next' or 'prev'.
halfEdgeConstructBoundary :: HalfEdge s -> ST s (CircularVector (HalfEdge s))
halfEdgeConstructBoundary halfEdge = trace ("mkBoundary from: " ++ show halfEdge) $ do
  tmp <- newVector 10
  writeVector tmp 0 halfEdge
  let loop i edge | edge == halfEdge = trace "Done" $ return i
      loop i edge = trace ("Going to: " ++ show edge) $ do
        face <- halfEdgeFace edge
        if faceIsInvalid face
          then do
            writeVector tmp i edge
            loop (i+1) =<< (halfEdgeTwin <$> halfEdgeNextIncoming edge)
          else
            loop i =<< (halfEdgeTwin <$> halfEdgePrev edge)
  i <- loop 1 =<< (halfEdgeTwin <$> halfEdgeNextIncoming halfEdge)
  cv <- freezeCircularVector i tmp
  trace ("Boundary: " ++ show cv) $ pure cv

-- | O(1)
halfEdgeIsInterior :: HalfEdge s -> ST s Bool
halfEdgeIsInterior edge = faceIsInterior <$> halfEdgeFace edge

-- O(1) Allocate new half-edge pair.
halfEdgeNew :: PlanarGraph s -> ST s (HalfEdge s)
halfEdgeNew pg = do
  eId <- readSTRef (pgNextHalfEdgeId pg)
  writeSTRef (pgNextHalfEdgeId pg) (eId+1)
  return (HalfEdge (eId*2) pg)

halfEdgeSetNext :: HalfEdge s -> HalfEdge s -> ST s ()
halfEdgeSetNext (HalfEdge e pg) (HalfEdge next pg') = eqCheck "halfEdgeSetNext" pg pg' $
  trace ("Set next: " ++ show (e, next)) $
  writeVector (pgHalfEdgeNext pg) e next

halfEdgeSetPrev :: HalfEdge s -> HalfEdge s -> ST s ()
halfEdgeSetPrev (HalfEdge e pg) (HalfEdge prev pg') = eqCheck "halfEdgeSetPrev" pg pg' $
  trace ("Set prev: " ++ show (e, prev)) $
  writeVector (pgHalfEdgePrev pg) e prev

halfEdgeSetFace :: HalfEdge s -> Face s -> ST s ()
halfEdgeSetFace (HalfEdge e pg) face =
  trace ("Set face: " ++ show (e, face)) $
  writeVector (pgHalfEdgeFace pg) e (faceToId face)

halfEdgeSetVertex :: HalfEdge s -> Vertex s -> ST s ()
halfEdgeSetVertex (HalfEdge e pg) vertex =
  writeVector (pgHalfEdgeVertex pg) e (vertexToId vertex)

-------------------------------------------------------------------------------
-- Faces

-- | O(1)
faceInvalid :: PlanarGraph s -> Face s
faceInvalid = faceFromId maxBound

-- | O(1)
faceIsValid :: Face s -> Bool
faceIsValid = not . faceIsInvalid

-- | O(1)
faceIsInvalid :: Face s -> Bool
faceIsInvalid (Face fId _) = fId == maxBound
faceIsInvalid (Boundary fId _) = fId == maxBound

-- | O(1)
faceFromId :: FaceId -> PlanarGraph s -> Face s
faceFromId fId pg | fId < 0 = Boundary (negate fId + 1) pg
faceFromId fId pg = Face fId pg

-- | O(1)
faceToId :: Face s -> FaceId
faceToId (Face fId _) = fId
faceToId (Boundary fId _) = negate fId - 1

-- | O(1)
faceHalfEdge         :: Face s -> ST s (HalfEdge s)
faceHalfEdge (Face fId pg) = do
  eId <- readVector (pgFaces pg) fId
  pure $ HalfEdge eId pg
faceHalfEdge (Boundary fId pg) = do
  eId <- readVector (pgBoundaries pg) fId
  pure $ HalfEdge eId pg

-- | O(1)
faceIsInterior       :: Face s -> Bool
faceIsInterior = not . faceIsBoundary

-- | O(1)
faceIsBoundary       :: Face s -> Bool
faceIsBoundary Face{}     = False
faceIsBoundary Boundary{} = True

-- faceVertices         :: Face s -> ST s (CircularVector (Vertex s))

-- | O(k)
--   Counterclockwise vector of edges.
faceHalfEdges        :: Face s -> ST s (CircularVector (HalfEdge s))
faceHalfEdges face
  | faceIsBoundary face = worker halfEdgeNext
  | otherwise           = worker halfEdgePrev
  where
    worker advance = do
      first <- faceHalfEdge face
      tmp <- newVector 10
      writeVector tmp 0 first
      let loop i edge | edge == first = return i
          loop i edge = do
            writeVector tmp i edge
            loop (i+1) =<< advance edge
      i <- loop 1 =<< advance first
      freezeCircularVector i tmp

-- | O(k)
faceBoundary :: Face s -> ST s (CircularVector (Vertex s))
faceBoundary face = CV.mapM halfEdgeVertex =<< faceHalfEdges face

-- faceAdjacentFaces    :: Face s -> ST s (CircularVector (Face s))

faceNew :: PlanarGraph s -> ST s (Face s)
faceNew pg = do
  fId <- readSTRef (pgNextFaceId pg)
  writeSTRef (pgNextFaceId pg) (fId+1)
  return (Face fId pg)

faceNewBoundary :: PlanarGraph s -> ST s (Face s)
faceNewBoundary pg = do
  fId <- readSTRef (pgNextBoundaryId pg)
  writeSTRef (pgNextBoundaryId pg) (fId+1)
  return (Boundary fId pg)

faceSetHalfEdge :: Face s -> HalfEdge s -> ST s ()
faceSetHalfEdge (Boundary fId pg) (HalfEdge eId pg') = eqCheck "faceSetHalfEdge" pg pg' $
  trace ("faceSetHalfEdge: " ++ show (fId, eId)) $
  writeVector (pgBoundaries pg) fId eId
faceSetHalfEdge (Face fId pg) (HalfEdge eId pg') = eqCheck "faceSetHalfEdge" pg pg' $
  writeVector (pgFaces pg) fId eId

-------------------------------------------------------------------------------
-- Mutation

pgConnectVertices :: HalfEdge s -> HalfEdge s -> ST s (Edge s)
pgConnectVertices e1 e2 = do
  -- Check e1.face == e2.face
  -- Check e1.next /= e2
  -- Check e2.next /= e1
  -- create new half-edge pair: e and e'
  -- e.vertex = e1.vertex
  -- e.next = e2
  --
  undefined

-- pgSplitHalfEdge :: HalfEdge s -> ST s (Vertex s)

-- pgRemoveFace :: Face s -> ST s ()
-- pgRemoveHalfEdge :: HalfEdge s -> ST s ()
-- pgRemoveVertex :: Vertex s -> ST s ()

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

tutteEmbedding :: PlanarGraph s -> ST s (Vector.Vector (Double, Double))
tutteEmbedding pg = do
  nVertices <- readSTRef (pgNextVertexId pg)
  trace ("nVertices: " ++ show nVertices) $ return ()
  m <- Vector.replicateM nVertices (V.replicate nVertices 0)
  vx <- V.replicate nVertices 0
  vy <- V.replicate nVertices 0

  boundary <- faceBoundary (Boundary 0 pg)
  let nBoundary = length boundary
  trace ("Vectors: " ++ show boundary) $ CV.forM_ (CV.zip boundary (circlePoints nBoundary)) $ \(vertex,(x,y)) -> do
    V.write (m Vector.! vertexToId vertex) (vertexToId vertex) (1::Double)
    V.write vx (vertexToId vertex) x
    V.write vy (vertexToId vertex) y

  forM_ [0..nVertices-1] $ \vId -> trace ("Vertex: " ++ show vId) $ do
    onBoundary <- vertexIsBoundary (vertexFromId vId pg)
    unless onBoundary $ do
      let vertex = vertexFromId vId pg
      neighbours <- vertexNeighbours vertex
      CV.forM_ neighbours $ \neighbour ->
        V.write (m Vector.! vId) (vertexToId neighbour) (1::Double)
      V.write (m Vector.! vId) vId (negate $ fromIntegral $ length neighbours)

  mi <- mapM Vector.freeze m
  vxi <- Vector.freeze vx
  vyi <- Vector.freeze vy

  let xPos = reifyMatrix mi vxi luSolve
      yPos = reifyMatrix mi vyi luSolve

  traceShow (mi, vxi,vyi) $ pure $ Vector.zip xPos yPos

reifyMatrix :: forall a. Vector.Vector (Vector.Vector a) ->
  Vector.Vector a ->
  (forall (n :: *). Dim n => V n (V n a) -> V n a -> V n a) ->
  Vector.Vector a
reifyMatrix m v f = reifyDim (Vector.length m) $ \(Proxy :: Proxy n) ->
  toVector (f (coerce m :: (V n (V n a))) (coerce v))

circlePoints :: Int -> CircularVector (Double, Double)
circlePoints n = CV.unsafeFromList
    [ (cos ang, sin ang)
    | i <- [0 .. n-1]
    , let ang = fromIntegral i * frac + pi/2]
  where
    frac = 2*pi / fromIntegral n

{-
0: (0,0)
1: (2,0)
2: (1,2)
3: (?,?)
4: (?,?)

3 = (0+1+2+4)/4
(3*4)-0-1-2-4 = 0

0   1   2   3   4
1   0   0   0    0      = 0
0   1   0   0    0      = 2
0   0   1   0    0      = 1
1/4 1/4 1/4 -1   1/4    = 0
1/3 1/3 0   1/3 -1      = 0

0   1   2   3   4
1   0   0   0    0      = 0
0   1   0   0    0      = 0
0   0   1   0    0      = 2
1/4 1/4 1/4 -1   1/4    = 0
1/3 1/3 0   1/3 -1      = 0


4 = (0+1+3)/3
-}
