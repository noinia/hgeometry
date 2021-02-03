module Data.PlanarGraph.Internal where

import           Control.Monad.ST
import           Data.STRef
import qualified Data.Vector          as V (unsafeFreeze)
import qualified Data.Vector          as Vector
import           Data.Vector.Circular (CircularVector)
import qualified Data.Vector.Circular as CV
import           Data.Vector.Mutable  (STVector)
import qualified Data.Vector.Mutable  as V

-------------------------------------------------------------------------------
-- Resizeable vector

type GrowVector s v = STRef s (STVector s v)

newVector :: Int -> ST s (GrowVector s v)
newVector n = newSTRef =<< V.new n

setVector :: GrowVector s v -> v -> ST s ()
setVector ref val = do
  vec <- readSTRef ref
  V.set vec val

readVector :: GrowVector s v -> Int -> ST s v
readVector ref idx = do
  v <- readSTRef ref
  V.read v idx

writeVector :: GrowVector s v -> Int -> v -> ST s ()
writeVector ref idx val = do
  v <- readSTRef ref
  let l = V.length v
  if idx >= l
    then {-trace ("Growing: " ++ show (idx, l)) $ -} do
      v' <- V.grow v ((idx+1)*2)
      V.write v' idx val
      writeSTRef ref v'
    else -- trace ("Writing: " ++ show (idx, l)) $
      V.write v idx val

freezeVector :: GrowVector s v -> ST s (Vector.Vector v)
freezeVector ref = Vector.freeze =<< readSTRef ref

unsafeFreezeVector :: GrowVector s v -> ST s (Vector.Vector v)
unsafeFreezeVector ref = Vector.unsafeFreeze =<< readSTRef ref

thawVector :: Vector.Vector v -> ST s (GrowVector s v)
thawVector v = newSTRef =<< Vector.thaw v

unsafeThawVector :: Vector.Vector v -> ST s (GrowVector s v)
unsafeThawVector v = newSTRef =<< Vector.unsafeThaw v


freezeCircularVector :: Int -> GrowVector s v -> ST s (CircularVector v)
freezeCircularVector n ref =
  (CV.unsafeFromVector . Vector.take n) <$> (V.unsafeFreeze =<< readSTRef ref)


-------------------------------------------------------------------------------
-- Planar graph

type HalfEdgeId = Int
type VertexId = Int
type FaceId = Int

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
  , pgVertexEdges    :: !(GrowVector s HalfEdgeId) -- Vertex indexed
  , pgFaceEdges      :: !(GrowVector s HalfEdgeId) -- Face indexed
  , pgBoundaryEdges  :: !(GrowVector s HalfEdgeId) -- Boundary faces
  } deriving Eq

