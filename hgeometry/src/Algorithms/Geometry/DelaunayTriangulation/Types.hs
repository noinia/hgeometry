{-# LANGUAGE ScopedTypeVariables #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Algorithms.Geometry.DelaunayTriangulation.Types
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Defines some geometric types used in the delaunay triangulation
--
--------------------------------------------------------------------------------
module Algorithms.Geometry.DelaunayTriangulation.Types
  ( VertexID
  , Vertex
  , Adj
  , Triangulation(..)
  , vertexIds
  , positions
  , neighbours
  , Mapping
  , edgesAsPoints
  , edgesAsVertices
  , toPlanarSubdivision
  , toPlaneGraph
  ) where

import           Control.Lens
import qualified Data.CircularList as C
import           Data.Ext
import           Data.Geometry
import           Data.Geometry.PlanarSubdivision
import qualified Data.IntMap.Strict as IM
import qualified Data.Map as M
-- import qualified Data.Map.Strict as SM
import qualified Data.PlaneGraph  as PG
import qualified Data.PlanarGraph as PPG
import qualified Data.Vector as V


--------------------------------------------------------------------------------

-- We store all adjacency lists in clockwise order

-- : If v on the convex hull, then its first entry in the adj. lists is its CCW
-- successor (i.e. its predecessor) on the convex hull

-- | Vertex identifier.
type VertexID = Int

-- | Rotating Right <-> rotate clockwise
type Vertex    = C.CList VertexID

-- | Neighbours indexed by VertexID.
type Adj = IM.IntMap (C.CList VertexID)

-- | Neighbours are stored in clockwise order: i.e. rotating right moves to the
-- next clockwise neighbour.
data Triangulation p r = Triangulation { _vertexIds  :: M.Map (Point 2 r) VertexID
                                       , _positions  :: V.Vector (Point 2 r :+ p)
                                       , _neighbours :: V.Vector (C.CList VertexID)
                                       }
                         deriving (Show,Eq)

-- | Mapping between triangulated points and their internal VertexID.
vertexIds :: Lens' (Triangulation p r) (M.Map (Point 2 r) VertexID)
vertexIds = lens _vertexIds (\(Triangulation _v p n) v -> Triangulation v p n)

-- | Point positions indexed by VertexID.
positions :: Lens (Triangulation p1 r) (Triangulation p2 r) (V.Vector (Point 2 r :+ p1)) (V.Vector (Point 2 r :+ p2))
positions = lens _positions (\(Triangulation v _p n) p -> Triangulation v p n)

-- | Point neighbours indexed by VertexID.
neighbours :: Lens' (Triangulation p r) (V.Vector (C.CList VertexID))
neighbours = lens _neighbours (\(Triangulation v p _n) n -> Triangulation v p n)


type instance NumType   (Triangulation p r) = r
type instance Dimension (Triangulation p r) = 2

-- | Bidirectional mapping between points and VertexIDs.
type Mapping p r = (M.Map (Point 2 r) VertexID, V.Vector (Point 2 r :+ p))




-- showDT :: (Show p, Show r)  => Triangulation p r -> IO ()
-- showDT = mapM_ print . edgesAsPoints

{- HLINT ignore edgesAsPoints -}
-- | List add edges as point pairs.
edgesAsPoints   :: Triangulation p r -> [(Point 2 r :+ p, Point 2 r :+ p)]
edgesAsPoints t = let pts = _positions t
                       in map (\(u,v) -> (pts V.! u, pts V.! v)) . edgesAsVertices $ t

-- | List add edges as VertexID pairs.
edgesAsVertices :: Triangulation p r -> [(VertexID,VertexID)]
edgesAsVertices = concatMap (\(i,ns) -> map (i,) . filter (> i) . C.toList $ ns)
       . zip [0..] . V.toList . _neighbours

--------------------------------------------------------------------------------

-- data ST a b c = ST { fst' :: !a, snd' :: !b , trd' :: !c}

-- type ArcID = Int

-- | ST' is a strict triple (m,a,x) containing:
--
-- - m: a Map, mapping edges, represented by a pair of vertexId's (u,v) with
--            u < v, to arcId's.
-- - a: the next available unused arcID
-- - x: the data value we are interested in computing
-- type ST' a = ST (SM.Map (VertexID,VertexID) ArcID) ArcID a


-- | convert the triangulation into a planarsubdivision
--
-- running time: \(O(n)\).
toPlanarSubdivision    :: (Ord r, Fractional r)
                       => proxy s -> Triangulation p r -> PlanarSubdivision s p () () r
toPlanarSubdivision px = fromPlaneGraph . toPlaneGraph px

-- | convert the triangulation into a plane graph
--
-- running time: \(O(n)\).
toPlaneGraph    :: forall proxy s p r.
                   proxy s -> Triangulation p r -> PG.PlaneGraph s p () () r
toPlaneGraph _ tr = PG.PlaneGraph $ g&PPG.vertexData .~ vtxData
  where
    g       = PPG.fromAdjacencyLists . V.toList . V.imap f $ tr^.neighbours
    f i adj = (VertexId i, C.leftElements $ VertexId <$> adj) -- report in CCW order
    vtxData = (\(loc :+ p) -> VertexData loc p) <$> tr^.positions
