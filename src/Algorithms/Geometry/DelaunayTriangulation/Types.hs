{-# LANGUAGE TemplateHaskell #-}
module Algorithms.Geometry.DelaunayTriangulation.Types where

import Data.Monoid(mempty)
import Control.Lens
import qualified Data.IntMap.Strict as IM
import qualified Data.Vector as V
import qualified Data.Map as M
import qualified Data.CircularList as C
import Data.Ext
import Data.Geometry
import Data.Geometry.Ipe

--------------------------------------------------------------------------------

-- We store all adjacency lists in clockwise order

-- : If v on the convex hull, then its first entry in the adj. lists is its CCW
-- successor (i.e. its predecessor) on the convex hull

-- | Rotating Right <-> rotate clockwise

type VertexID = Int

type Vertex    = C.CList VertexID

type Adj = IM.IntMap (C.CList VertexID)

-- | Neighbours are stored in clockwise order: i.e. rotating right moves to the
-- next clockwise neighbour.
data Triangulation p r = Triangulation { _vertexIds  :: M.Map (Point 2 r) VertexID
                                       , _positions  :: V.Vector (Point 2 r :+ p)
                                       , _neighbours :: V.Vector (C.CList VertexID)
                                       }
                         deriving (Show,Eq)
makeLenses ''Triangulation


type Mapping p r = (M.Map (Point 2 r) VertexID, V.Vector (Point 2 r :+ p))




showDT :: (Show p, Show r)  => Triangulation p r -> IO ()
showDT = mapM_ print . edges


edges   :: Triangulation p r -> [(Point 2 r :+ p, Point 2 r :+ p)]
edges t = let pts = _positions t
          in map (\(u,v) -> (pts V.! u, pts V.! v)) . edges' $ t


edges' :: Triangulation p r -> [(VertexID,VertexID)]
edges' = concatMap (\(i,ns) -> map (i,) . filter (> i) . C.toList $ ns)
       . zip [0..] . V.toList . _neighbours

drawTriangulation :: IpeOut (Triangulation p r) (IpeObject r)
drawTriangulation = IpeOut $ \tr ->
    let es = map (uncurry ClosedLineSegment) . edges $ tr
    in asIpeGroup $ map (\e -> asIpeObjectWith ipeLineSegment e mempty) es
