{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
module Algorithms.Geometry.DelaunayTriangulation.DivideAndConquereor where

import Control.Monad.State
import Control.Monad.Reader
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
import Data.Geometry.Polygon.Convex(focus', pred', succ')
import Data.Ext
import Data.Geometry
import Data.Geometry.Interval
import Data.Geometry.Polygon
import Data.Geometry.Ball(disk, inClosedBall)

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


delaunayTriangulation      :: (Ord r, Fractional r)
                           => NonEmpty.NonEmpty (Point 2 r :+ p) -> Triangulation p r
delaunayTriangulation pts' = Triangulation vtxMap ptsV adjV
  where
    pts    = NonEmpty.sortBy (compare `on` (^.core)) pts'
    ptsV   = V.fromList . F.toList $ pts
    vtxMap = M.fromList $ zip (map (^.core) . V.toList $ ptsV) [0..]

    tr     = fmap _unElem $ asBalancedBinLeafTree pts

    (adj,_) = delaunayTriangulation' tr (vtxMap,ptsV)
    adjV    = V.fromList . IM.elems $ adj


type Adj = IM.IntMap (C.CList VertexID)

-- : pre: - Input points are sorted lexicographically


delaunayTriangulation'   :: (Ord r, Fractional r)
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



merge           :: (Ord r, Fractional r) => Adj
                -> Adj
                -> LineSegment 2 p r
                -> LineSegment 2 p r
                -> Mapping p r
                -> Adj
merge ld rd bt ut mapping@(vtxMap,_) = let l   = lookup' vtxMap (bt^.start.core)
                                           r   = lookup' vtxMap (bt^.end.core)
                                           tl  = lookup' vtxMap (ut^.end.core)
                                           tr  = lookup' vtxMap (ut^.end.core)
                                           adj = ld `IM.union` rd
                                       in flip runReader mapping . flip execStateT adj $
                                          moveUp (tl,tr) l r


type Vertex    = C.CList VertexID
type Merge p r = StateT Adj (Reader (Mapping p r))

moveUp :: (Ord r, Fractional r)
                     => (VertexID,VertexID) -> VertexID -> VertexID -> Merge p r ()
moveUp ut l r
  | (l,r) == ut = pure ()
  | otherwise   = do
                     -- Get the neighbours of r and l along the convex hull
                     r1 <- fromJust . IM.lookup r <$> get
                     l1 <- fromJust . IM.lookup l <$> get
                     insert l r
                     (r1',a) <- rotateR l r r1
                     (l1',b) <- rotateL l r l1
                     c       <- qTest l r r1' l1'
                     let (l',r') = case (a,b,c) of
                                     (True,_,_)          -> (focus' l1', r)
                                     (False,True,_)      -> (l,          focus' r1')
                                     (False,False,True)  -> (l,          focus' r1')
                                     (False,False,False) -> (focus' l1', r)
                     moveUp ut l' r'

-- | ''rotates'' around r and removes all neighbours of r that violate the
-- delaunay condition. Returns the first vertex (as a Neighbour of r) that
-- should remain in the Delaunay Triangulation, as well as a boolean A that
-- helps deciding if we merge up by rotating left or rotating right (See
-- description in the paper for more info)
rotateR     :: (Ord r, Fractional r)
                     => VertexID -> VertexID -> Vertex -> Merge p r (Vertex, Bool)
rotateR l r r1 = focus' r1 `isLeftOf` (l, r) >>= \case
                   True  -> (,False) <$> rotateR' l r r1 (pred' r1)
                   False -> pure (r1,True)

-- | The code that does the actual rotating
rotateR'             :: (Ord r, Fractional r)
                     => VertexID -> VertexID -> Vertex -> Vertex -> Merge p r Vertex
rotateR' l r r1' r2' = go r1' r2'
  where
    go r1 r2 = qTest l r r1 r2 >>= \case
                 True  -> pure r1
                 False -> do modify $ delete r (focus' r1)
                             go r2 (pred' r1)

-- | Symmetric to rotateR
rotateL     :: (Ord r, Fractional r)
                     => VertexID -> VertexID -> Vertex -> Merge p r (Vertex, Bool)
rotateL l r l1 = focus' l1 `isRightOf` (r, l) >>= \case
                   True  -> (,False) <$> rotateL' l r l1 (succ' l1)
                   False -> pure (l1,True)

-- | The code that does the actual rotating. Symmetric to rotateR'
rotateL'             :: (Ord r, Fractional r)
                     => VertexID -> VertexID -> Vertex -> Vertex -> Merge p r Vertex
rotateL' l r l1' l2' = go l1' l2'
  where
    go l1 l2 = qTest l r l1 l2 >>= \case
                 True  -> pure l1
                 False -> do modify $ delete l (focus' l1)
                             go l2 (succ' l1)



--------------------------------------------------------------------------------
-- * Primitives used by the Algorithm

-- | returns True if the forth point (vertex) does not lie in the disk defined
-- by the first three points.
qTest         :: (Ord r, Fractional r)
              => VertexID -> VertexID -> Vertex -> Vertex -> Merge p r Bool
qTest h i j k = withPtMap . snd <$> ask
  where
    withPtMap ptMap = let h' = ptMap V.! h
                          i' = ptMap V.! i
                          j' = ptMap V.! (focus' j)
                          k' = ptMap V.! (focus' k)
                      in not . maybe True ((k'^.core) `inClosedBall`) $ disk' h' i' j'
    disk' p q r = disk (p^.core) (q^.core) (r^.core)

-- | Inserts an edge into the right position.
insert     :: (Num r, Ord r) => VertexID -> VertexID -> Merge p r ()
insert u v = ask >>= modify . insert' u v

-- | Inserts an edge (and makes sure that the vertex is inserted in the
-- correct. pos in the adjacency lists)
insert'               :: (Num r, Ord r)
                      => VertexID -> VertexID -> Mapping p r -> Adj -> Adj
insert' u v (_,ptMap) = IM.adjustWithKey (insert' v) u . IM.adjustWithKey (insert' u) v
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




-- | Lifted version of Convex.IsLeftOf
isLeftOf           :: (Ord r, Num r)
                   => VertexID -> (VertexID, VertexID) -> Merge p r Bool
p `isLeftOf` (l,r) = withPtMap . snd <$> ask
  where
    withPtMap ptMap = (ptMap V.! p) `Convex.isLeftOf` (ptMap V.! l, ptMap V.! r)

-- | Lifted version of Convex.IsRightOf
isRightOf :: (Ord r, Num r) => VertexID -> (VertexID, VertexID) -> Merge p r Bool
p `isRightOf` (l,r) = withPtMap . snd <$> ask
  where
    withPtMap ptMap = (ptMap V.! p) `Convex.isRightOf` (ptMap V.! l, ptMap V.! r)

--------------------------------------------------------------------------------
-- * Some Helper functions


lookup'     :: Ord k => M.Map k a -> k -> a
lookup' m x = fromJust $ M.lookup x m

size'              :: BinLeafTree Size a -> Size
size' (Leaf _)     = 1
size' (Node _ s _) = s

--------------------------------------------------------------------------------
