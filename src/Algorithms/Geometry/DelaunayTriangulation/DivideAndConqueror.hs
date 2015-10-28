module Algorithms.Geometry.DelaunayTriangulation.DivideAndConquereor where

import qualified Data.Foldable as F
import Data.Maybe(fromJust)
import qualified Data.Vector as V
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.IntMap.Strict as IM


import Data.Ext

import Data.Geometry
import Data.Geometry.Polygon.Convex
import Data.BinaryTree



-- We store all adjacency lists in clockwise order

-- : If v on the convex hull, then its first entry in the adj. lists is its CCW
-- successor (i.e. its predecessor) on the convex hull


type VertexID = Int

data Triangulation p r = Triangulation { _vertexIds  :: M.Map (Point 2 r) VertexID
                                       , _points     :: V.Vector (Point 2 r :+ p)
                                       , _neighbours :: V.Vector (C.CList VertexID)
                                       }

type Mapping p r = (M.Map (Point 2 r) VertexID, V.VertexID (Point 2 r :+ p))


data TriangRes p r = TriangRes { _triang     :: Triangulation p r
                               , _convexHull :: ConvexPolygon p r
                               }


delaunayTriangulation      :: [Point 2 r :+ p] -> Triangulation p r
delaunayTriangulation pts' =
  where
    pts = L.sortBy (compare `on` (^.core)) pts'
    tr  = asBalancedBinLeafTree $ pts'


-- : pre: - Input points are sorted lexicographically
--        - at least three points

delaunayTriangulation'   :: BinLeafTree Size (Point 2 r :+ p)
                         -> Mapping p r
                         -> IM.IntMap (C.CList VertexID)
                         -- adjacency list representation of the sub-triangulation
                         -> (IM.IntMap (C.CList VertexID), ConvexPolygon () r)
delaunayTriangulation' pts (vtxMap,_)
  | size pts == 3 = let pts'@[p,q,r] = F.toList pts
                        [pi,qi,ri]   = map (lookup' vtxMap) pts'
                        IM.fromList [ (p, C.singleton q)
                                    , (q, C.singleton r)
                                    , (r, C.singleton q)
                                    ]
                    in
  | otherwise     =
  undefined


lookup' m x = fromJust $ M.lookup x m
