module Algorithms.Geometry.DelaunayTriangulation.DivideAndConquereor where

import Control.Lens
import Data.Function(on)
import qualified Data.Foldable as F
import Data.Maybe(fromJust)
import qualified Data.Vector as V
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as M
import qualified Data.IntMap.Strict as IM
import qualified Data.CircularList as C
import Data.Geometry.Polygon.Convex(ConvexPolygon)
import qualified Data.Geometry.Polygon.Convex as Convex
import Data.Ext
import Data.Geometry
import Data.Geometry.Polygon
import Data.BinaryTree


--------------------------------------------------------------------------------

-- Implementation of the Divide & Conqueror algorithm as described in:
--
-- Two Algorithms for Constructing a Delaunay Triangulation
-- Lee and Schachter
-- International Journal of Computer and Information Sciences, Vol 9, No. 3, 1980

-- We store all adjacency lists in clockwise order

-- : If v on the convex hull, then its first entry in the adj. lists is its CCW
-- successor (i.e. its predecessor) on the convex hull

-- | Rotating Right <-> rotate clockwise


type VertexID = Int




-- | Neighbours are stored in clockwise order: i.e. rotating right moves to the
-- next clockwise neighbour.
data Triangulation p r = Triangulation { _vertexIds  :: M.Map (Point 2 r) VertexID
                                       , _points     :: V.Vector (Point 2 r :+ p)
                                       , _neighbours :: V.Vector (C.CList VertexID)
                                       }



type Mapping p r = (M.Map (Point 2 r) VertexID, V.Vector (Point 2 r :+ p))


data TriangRes p r = TriangRes { _triang     :: Triangulation p r
                               , _convexHull :: ConvexPolygon p r
                               }


delaunayTriangulation      :: Ord r
                           => NonEmpty.NonEmpty (Point 2 r :+ p) -> Triangulation p r
delaunayTriangulation pts' = undefined
  where
    pts = NonEmpty.sortBy (compare `on` (^.core)) pts'
    tr  = asBalancedBinLeafTree pts

type Adj = IM.IntMap (C.CList VertexID)

-- : pre: - Input points are sorted lexicographically


delaunayTriangulation'   :: (Ord r, Num r)
                         => BinLeafTree Size (Point 2 r :+ p)
                         -> Mapping p r
                         -> (Adj, ConvexPolygon p r)
delaunayTriangulation' pts mapping@(vtxMap,_)
  | size' pts == 1 = let (Leaf p) = pts
                         pi       = lookup' vtxMap (p^.core)
                     in (IM.singleton pi C.empty, fromPoints [p])
  | size' pts == 2 = let pts'@[p,q] = F.toList pts
                         [pi,qi]    = map ((lookup' vtxMap) . (^.core)) pts'
                     in ( IM.fromList [ (pi, C.singleton qi)
                                      , (qi, C.singleton pi) ]
                        , fromPoints pts'
                        )
  | otherwise      = let (Node lt _ rt) = pts
                         (ld,lch)       = delaunayTriangulation' lt mapping
                         (rd,rch)       = delaunayTriangulation' rt mapping
                         (ch, bt, ut)   = Convex.merge lch rch
                     in (merge ld rd bt ut mapping, ch)



merge           :: Adj
                -> Adj
                -> LineSegment 2 p r
                -> LineSegment 2 p r
                -> Mapping p r
                -> Adj
merge ld rd bt ut mapping@(vtxMap,_) = undefined


-- | Inserts an edge (and makes sure that the vertex is inserted in the
-- correct. pos in the adjacency lists)
insert               :: (Num r, Ord r)
                     => Mapping p r -> VertexID -> VertexID -> Adj -> Adj
insert (_,ptMap) u v = IM.adjustWithKey (insert' v) u . IM.adjustWithKey (insert' u) v
  where
    -- inserts b into the adjacency list of a
    insert' ai bi adjA = case C.toList adjA of
        []   -> C.singleton bi
        [ci] -> C.fromList [bi,ci]
        _    -> let a = ptMap V.! ai
                    b = ptMap V.! bi
                in C.insertL bi . Convex.rotateRWhile (\_ di ->
                       ptMap V.! di `Convex.isRightOf` (a,b)) $ adjA

-- | Deletes an edge
delete     :: VertexID -> VertexID -> Adj -> Adj
delete u v = IM.adjust (delete' v) u . IM.adjust (delete' u) v
  where
    delete' x = C.filterL (/= x) -- should we rotate left or right if it is the focus?

lookup' m x = fromJust $ M.lookup x m

size'    :: BinLeafTree Size a -> Size
size' (Leaf _)     = 1
size' (Node _ s _) = s
