module Data.PlanarGraph.Mutable where

import Data.Bits
import Control.Monad.ST
import Control.Monad
import Data.STRef
import qualified Data.Vector as V (unsafeFreeze)
import qualified Data.Vector.Mutable as V
import Data.Vector.Mutable (STVector)
import qualified Data.Vector.Circular as CV
import Data.Vector.Circular (CircularVector)

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
    then do
      v' <- V.grow v (idx*2)
      V.write v' idx val
      writeSTRef ref v'
    else V.write v idx val

freezeCircularVector :: GrowVector s v -> ST s (CircularVector v)
freezeCircularVector ref = CV.unsafeFromVector <$> (V.unsafeFreeze =<< readSTRef ref)


-------------------------------------------------------------------------------
-- Elements: Half-edges, vertices, faces.



type HalfEdgeId = Int
data HalfEdge s = HalfEdge HalfEdgeId (PlanarGraph s)
  deriving Eq

data Edge s = Edge Int (PlanarGraph s)
  deriving Eq

type VertexId = Int
data Vertex s = Vertex VertexId (PlanarGraph s)
  deriving Eq

type FaceId = Int
data Face s = Face FaceId (PlanarGraph s)
  deriving Eq

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
  { pgNextHalfEdgeId     :: !(STRef s HalfEdgeId)
  , pgNextVertexId       :: !(STRef s VertexId)
  , pgNextFaceId         :: !(STRef s FaceId)
  , pgHalfEdgeNext       :: !(GrowVector s HalfEdgeId) -- HalfEdge indexed
  , pgHalfEdgePrev       :: !(GrowVector s HalfEdgeId) -- HalfEdge indexed
  , pgHalfEdgeVertex     :: !(GrowVector s VertexId)   -- HalfEdge indexed
  , pgHalfEdgeFace       :: !(GrowVector s FaceId)     -- HalfEdge indexed
  , pgVertices           :: !(GrowVector s HalfEdgeId) -- Vertex indexed
  , pgFaces              :: !(GrowVector s HalfEdgeId) -- Face indexed
  } deriving Eq

panic :: String -> String -> a
panic tag msg = error $ "Data.PlanarGraph.Mutable." ++ tag ++ ": " ++ msg

eqCheck :: String -> PlanarGraph s -> PlanarGraph s -> a -> a
eqCheck tag pg1 pg2 v
  | pg1 == pg2 = v
  | otherwise = panic tag "Invalid cross reference."

empty :: Int -> ST s (PlanarGraph s)
empty size = PlanarGraph
  <$> newSTRef 0
  <*> newSTRef 0
  <*> newSTRef 0
  <*> newVector size
  <*> newVector size
  <*> newVector size
  <*> newVector size
  <*> newVector size
  <*> newVector size

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
new 0 = empty 0
new 1 = undefined
new 2 = undefined
new n = do
  pg <- empty n
  exteriorFace <- faceNew pg
  innerFace <- faceNew pg

  edges <- CV.unsafeFromList <$> Control.Monad.replicateM n (halfEdgeNew pg)
  let edgeTriples = CV.zip3 (CV.rotateLeft 1 edges) edges (CV.rotateRight 1 edges)
  vertices <- CV.unsafeFromList <$> replicateM n (vertexNew pg)

  forM_ (edgeTriples) $ \(prev, edge, next) -> do
    halfEdgeSetNext edge next
    halfEdgeSetPrev edge prev
    halfEdgeSetFace edge exteriorFace
    halfEdgeSetNext (halfEdgeTwin edge) (halfEdgeTwin prev)
    halfEdgeSetPrev (halfEdgeTwin edge) (halfEdgeTwin next)
    halfEdgeSetFace (halfEdgeTwin edge) innerFace
  forM_ (CV.zip edges vertices) $ \(edge, vertex) -> do
    halfEdgeSetVertex edge vertex
  forM_ (CV.zip edges (CV.rotateRight 1 vertices)) $ \(edge, vertex) -> do
    halfEdgeSetVertex (halfEdgeTwin edge) vertex
    vertexSetHalfEdge vertex (halfEdgeTwin edge)

  faceSetHalfEdge innerFace (CV.head edges)
  faceSetHalfEdge exteriorFace (halfEdgeTwin $ CV.head edges)

  return pg


-- | O(k)
--   Create N new vertices as a hole in face.
newHole :: Face s -> Int -> ST s ()
newHole = undefined

-- | O(n)
clone :: PlanarGraph s -> ST s (PlanarGraph s)
clone = undefined

-- dualTree :: Face s -> ST s (Tree (Face s))
-- dualTree = undefined

-------------------------------------------------------------------------------
-- Vertices

vertexFromId :: VertexId -> PlanarGraph s -> Vertex s
vertexFromId vId pg = Vertex vId pg

-- | O(1)
vertexHalfEdge :: Vertex s -> ST s (HalfEdge s)
vertexHalfEdge = undefined

-- | O(k)
vertexOutgoingHalfEdges :: Vertex s -> ST s [HalfEdge s]
vertexOutgoingHalfEdges = undefined

-- | O(k), more efficient than 'vertexOutgoingHalfEdges'.
vertexWithOutgoingHalfEdges :: Vertex s -> (HalfEdge s -> m ()) -> ST s ()
vertexWithOutgoingHalfEdges = undefined

-- | O(k)
vertexIncomingHalfEdges :: Vertex s -> ST s [HalfEdge s]
vertexIncomingHalfEdges = undefined

-- | O(k)
vertexWithIncomingHalfEdges :: Vertex s -> (HalfEdge s -> ST s ()) -> ST s ()
vertexWithIncomingHalfEdges = undefined

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
halfEdgeNext :: HalfEdge s -> ST s (HalfEdge s)
halfEdgeNext (HalfEdge eId pg) = do
  next <- readVector (pgHalfEdgeNext pg) eId
  pure $ HalfEdge next pg

-- | O(1)
halfEdgeVertex     :: HalfEdge s -> ST s (Vertex s)
halfEdgeVertex (HalfEdge idx pg) = do
  v <- readVector (pgHalfEdgeVertex pg) idx
  pure $ Vertex v pg

-- | O(1)
halfEdgeTwin       :: HalfEdge s -> HalfEdge s
halfEdgeTwin (HalfEdge idx graph) = HalfEdge (idx `xor` 1) graph

-- halfEdgeTailVertex :: HalfEdge s -> ST s (Vertex s)
-- halfEdgeTipVertex  :: HalfEdge s -> ST s (Vertex s)

-- | O(1)
halfEdgeFace       :: HalfEdge s -> ST s (Face s)
halfEdgeFace (HalfEdge eId pg) = do
  fId <- readVector (pgHalfEdgeFace pg) eId
  pure $ Face fId pg

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
  writeVector (pgHalfEdgeNext pg) e next

halfEdgeSetPrev :: HalfEdge s -> HalfEdge s -> ST s ()
halfEdgeSetPrev (HalfEdge e pg) (HalfEdge next pg') = eqCheck "halfEdgeSetPrev" pg pg' $
  writeVector (pgHalfEdgePrev pg) e next

halfEdgeSetFace :: HalfEdge s -> Face s -> ST s ()
halfEdgeSetFace (HalfEdge e pg) (Face face pg') = eqCheck "halfEdgeSetFace" pg pg' $
  writeVector (pgHalfEdgeFace pg) e face

halfEdgeSetVertex :: HalfEdge s -> Vertex s -> ST s ()
halfEdgeSetVertex (HalfEdge e pg) (Vertex vertex pg') = eqCheck "halfEdgeSetVertex" pg pg' $
  writeVector (pgHalfEdgeVertex pg) e vertex

-------------------------------------------------------------------------------
-- Faces

-- | O(1)
faceFromId :: FaceId -> PlanarGraph s -> Face s
faceFromId fId pg = Face fId pg

-- | O(1)
faceHalfEdge         :: Face s -> ST s (HalfEdge s)
faceHalfEdge (Face fId pg) = do
  eId <- readVector (pgFaces pg) fId
  pure $ HalfEdge eId pg

faceIsInterior       :: Face s -> Bool
faceIsInterior (Face fId _) = fId /= 0

faceIsExterior       :: Face s -> Bool
faceIsExterior (Face fId _) = fId == 0

-- faceVertices         :: Face s -> ST s (CircularVector (Vertex s))

-- | O(k)
--   Edges around the exterior are counterclockwise, edges around holes are clockwise.
faceHalfEdges        :: Face s -> ST s (CircularVector (HalfEdge s))
faceHalfEdges face = do
  first <- faceHalfEdge face
  tmp <- newVector 10
  writeVector tmp 0 first
  let loop _ edge | edge == first = return ()
      loop i edge = do
        writeVector tmp i edge
        loop (i+1) =<< halfEdgeNext edge
  loop 1 =<< halfEdgeNext first
  freezeCircularVector tmp
-- faceAdjacentFaces    :: Face s -> ST s (CircularVector (Face s))

faceNew :: PlanarGraph s -> ST s (Face s)
faceNew pg = do
  fId <- readSTRef (pgNextFaceId pg)
  writeSTRef (pgNextFaceId pg) (fId+1)
  return (Face fId pg)

faceSetHalfEdge :: Face s -> HalfEdge s -> ST s ()
faceSetHalfEdge (Face fId pg) (HalfEdge eId pg') = eqCheck "faceSetHalfEdge" pg pg' $ do
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
-- splitHalfEdge :: HalfEdge s -> ST s (Vertex s)

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

