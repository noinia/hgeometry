--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.DelaunayTriangulation.Types
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Defines some geometric types used in the delaunay triangulation
--
--------------------------------------------------------------------------------
module HGeometry.DelaunayTriangulation.Types
  ( VertexID
  , DTVertex
  , Adj
  , Triangulation(..)
  , vertexIds
  , positions
  , neighbours
  , Mapping
  , edgesAsPoints
  , edgesAsVertices
  -- , toPlanarSubdivision
  , toPlaneGraph
  ) where

import           Control.Lens
import qualified Data.CircularList as C
import           Data.Coerce
import qualified Data.IntMap.Strict as IM
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Vector as V
import qualified Data.Vector.NonEmpty as NonEmptyV
import           HGeometry.Ext
import           HGeometry.PlaneGraph
import           HGeometry.Point
import           HGeometry.Properties
import qualified Hiraffe.PlanarGraph as PPG

--------------------------------------------------------------------------------

-- We store all adjacency lists in clockwise order

-- : If v on the convex hull, then its first entry in the adj. lists is its CCW
-- successor (i.e. its predecessor) on the convex hull

-- | Vertex identifier.
type VertexID = Int

-- | Rotating Right <-> rotate clockwise
type DTVertex    = C.CList VertexID

-- | Neighbours indexed by VertexID.
type Adj = IM.IntMap (C.CList VertexID)

-- | Neighbours are stored in clockwise order: i.e. rotating right moves to the
-- next clockwise neighbour.
data Triangulation point = Triangulation { _vertexIds  :: Map.Map point VertexID
                                         , _positions  :: V.Vector point
                                         , _neighbours :: V.Vector (C.CList VertexID)
                                           -- ^ neighbours are in CW order
                                         }
                         deriving (Show,Eq)

-- data Triangulation p r = Triangulation { _vertexIds  :: Map.Map (Point 2 r) VertexID
--                                        , _positions  :: V.Vector (Point 2 r :+ p)
--                                        , _neighbours :: V.Vector (C.CList VertexID)
--                                        }
--                          deriving (Show,Eq)


-- | Mapping between triangulated points and their internal VertexID.
vertexIds :: Lens' (Triangulation point) (Map.Map point VertexID)
vertexIds = lens _vertexIds (\(Triangulation _v p n) v -> Triangulation v p n)

-- | Point positions indexed by VertexID.
positions :: Lens' (Triangulation point) (V.Vector point)
positions = lens _positions (\(Triangulation v _p n) p -> Triangulation v p n)

-- | Point neighbours indexed by VertexID.
neighbours :: Lens' (Triangulation point) (V.Vector (C.CList VertexID))
neighbours = lens _neighbours (\(Triangulation v p _n) n -> Triangulation v p n)


type instance NumType   (Triangulation point) = NumType point
type instance Dimension (Triangulation point) = 2

-- | Bidirectional mapping between points and VertexIDs.
type Mapping point = (Map.Map point VertexID, V.Vector point)


-- showDT :: (Show p, Show r)  => Triangulation point -> IO ()
-- showDT = mapM_ print . edgesAsPoints


-- | List add edges as point pairs.
edgesAsPoints   :: Triangulation point -> [(point,point)]
edgesAsPoints t = let pts = _positions t
                  in map (bimap (pts V.!) (pts V.!)) . edgesAsVertices $ t

-- | List add edges as VertexID pairs.
edgesAsVertices :: Triangulation point -> [(VertexID,VertexID)]
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


{-

-- | convert the triangulation into a planarsubdivision
--
-- running time: \(O(n)\).
toPlanarSubdivision :: forall s p r. (Ord r, Fractional r)
                    => Triangulation point -> PlanarSubdivision s p () () r
toPlanarSubdivision = fromPlaneGraph . toPlaneGraph
-}


-- | convert the triangulation into a plane graph
--
-- running time: \(O(n)\).
toPlaneGraph    :: forall s point r. (Point_ point 2 r, Ord r, Num r)
                => Triangulation point -> CPlaneGraph s point () ()
toPlaneGraph tr = fromAdjacencyLists
                . NonEmpty.fromList . V.toList -- TODO: direclty use a NonEmptyVector
                $ V.izipWith f (tr^.neighbours) (tr^.positions)
  where
    f i adj v = (coerce i, v, NonEmpty.fromList . C.leftElements $ (\j -> (coerce j, ())) <$> adj)
                -- report in CCW order


-- nonEmpty (vi, v, h (vi, e)) -> graph

--  undefined
{-
  PlaneGraph $ g&PPG.vertexData .~ vtxData
  where
    g       = PPG.fromAdjacencyLists . V.toList . V.imap f $ tr^.neighbours
    f i adj = (VertexId i, C.leftElements $ VertexId <$> adj) -- report in CCW order
    vtxData = (\(loc :+ p) -> VertexData loc p) <$> tr^.positions
-}
