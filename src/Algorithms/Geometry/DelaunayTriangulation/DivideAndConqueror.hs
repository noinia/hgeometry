{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
module Algorithms.Geometry.DelaunayTriangulation.DivideAndConquereor where

import Control.Monad.State
import Control.Monad.Reader
import Control.Lens hiding (only)
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
import Data.Geometry.Ball(disk, insideBall)

import Data.BinaryTree

--------------------------------------------------------------------------------


import Debug.Trace
import Data.Geometry.Ipe hiding (disk, lookup')

main = do
         Right (page :: IpePage Rational) <- readSinglePageFile "/Users/frank/tmp/dt.ipe"
         let syms = page^..content.traverse._IpeUse
             pts  = map (\s -> s&core %~ (^.symbolPoint)) syms
             dt  = delaunayTriangulation $ NonEmpty.fromList pts
         showDT dt

showDT :: (Show p, Show r)  => Triangulation p r -> IO ()
showDT = mapM_ print . edges


edges   :: Triangulation p r -> [(Point 2 r :+ p, Point 2 r :+ p)]
edges t = let pts = _positions t
          in map (\(u,v) -> (pts V.! u, pts V.! v)) . edges' $ t


edges' :: Triangulation p r -> [(VertexID,VertexID)]
edges' = concatMap (\(i,ns) -> map (i,) . filter (> i) . C.toList $ ns)
       . zip [0..] . V.toList . _neighbours



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
                                       , _positions  :: V.Vector (Point 2 r :+ p)
                                       , _neighbours :: V.Vector (C.CList VertexID)
                                       }
                         deriving (Show,Eq)
-- makeLenses ''Triangulation


type Mapping p r = (M.Map (Point 2 r) VertexID, V.Vector (Point 2 r :+ p))


data TriangRes p r = TriangRes { _triang     :: Triangulation p r
                               , _convexHull :: ConvexPolygon p r
                               }


delaunayTriangulation      :: (Ord r, Fractional r,       Show r, Show p)
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


delaunayTriangulation'   :: (Ord r, Fractional r,      Show r, Show p)
                         => BinLeafTree Size (Point 2 r :+ p)
                         -> Mapping p r
                         -> (Adj, ConvexPolygon p r)
delaunayTriangulation' pts mapping@(vtxMap,_)
  | trace ("DT " ++ show (size' pts)) False = undefined
  | size' pts == 1 = let (Leaf p) = pts
                         pi       = lookup' vtxMap (p^.core)
                     in (IM.singleton pi C.empty, fromPoints [p])
  | size' pts == 2 = let pts'@[p,q] = F.toList pts
                         [pi,qi]    = map ((lookup' vtxMap) . (^.core)) pts'
                     in ( IM.fromList [ (pi, C.singleton qi)
                                      , (qi, C.singleton pi) ]
                        , fromPoints pts'
                        )
  | size' pts == 3 = let pts'@[p,q,r] = F.toList pts
                         [pi,qi,ri]   = map ((lookup' vtxMap) . (^.core)) pts'
                     in ( IM.fromList [ (pi, C.fromList [qi, ri])
                                      , (qi, C.fromList [ri, pi])
                                      , (ri, C.fromList [pi, qi])
                                      ]      -- TODO: Check orientations..
                        , fromPoints pts'
                        )
  | otherwise      = let (Node lt _ rt) = pts
                         (ld,lch)       = delaunayTriangulation' lt mapping
                         (rd,rch)       = delaunayTriangulation' rt mapping
                         (ch, bt, ut)   = Convex.merge lch rch
                     in trace ("HULLS: " ++ show (lch,rch)) $

                       (merge ld rd bt ut mapping, ch)



merge           :: (Ord r, Fractional r,      Show r, Show p)
                   => Adj
                -> Adj
                -> LineSegment 2 p r
                -> LineSegment 2 p r
                -> Mapping p r
                -> Adj
merge ld rd bt ut mapping@(vtxMap,_)
  | trace ("Merge " ++ show (ld,rd,bt,ut)) False = undefined
  | otherwise                        = let l   = lookup' vtxMap (bt^.start.core)
                                           r   = lookup' vtxMap (bt^.end.core)
                                           tl  = lookup' vtxMap (ut^.start.core)
                                           tr  = lookup' vtxMap (ut^.end.core)
                                           adj = ld `IM.union` rd
                                       in flip runReader mapping . flip execStateT adj $
                                          moveUp (tl,tr) l r


type Vertex    = C.CList VertexID
type Merge p r = StateT Adj (Reader (Mapping p r))


lookup'' :: Show a => Int -> IM.IntMap a -> a
lookup'' k m | traceShow (m,k) False = undefined
lookup'' k m = fromJust . IM.lookup k $ m


moveUp :: (Ord r, Fractional r,      Show r, Show p)
                     => (VertexID,VertexID) -> VertexID -> VertexID -> Merge p r ()
moveUp ut l r
  | trace ("MERGE: UT: " ++ show ut) False = undefined
  | (l,r) == ut = insert l r
  | otherwise   = do
                     trace ("============ STARTING ITERATION WITH " ++ show (l,r)) (pure ())
                     -- Get the neighbours of r and l along the convex hull
                     r1 <- lookup'' r <$> get
                     l1 <- lookup'' l <$> get
                     trace ("NEIGHBORS OF " ++ show l ++ ": " ++ show l1) (pure ())
                     trace ("INSERTING " ++ show (l,r)) insert l r
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
rotateR l r r1 | trace ("rotR " ++ show (l,r,r1))               False = undefined
rotateR l r r1 = focus' r1 `isLeftOf` (l, r) >>= \case
                   True  -> (,False) <$> rotateR' l r r1 (pred' r1)
                   False -> pure (r1,True)

-- | The code that does the actual rotating
rotateR'             :: (Ord r, Fractional r)
                     => VertexID -> VertexID -> Vertex -> Vertex -> Merge p r Vertex
rotateR' l r r1' r2' | trace ("RR'" ++ show (l,r,r1',r2')) False = undefined
rotateR' l r r1' r2' = go r1' r2'
  where
    go r1 r2 | trace ("GR" ++ show (r1,r2)) False = undefined
    go r1 r2 = qTest l r r1 r2 >>= \case
                 True  -> pure r1
                 False -> do modify $ delete r (focus' r1)
                             go r2 (pred' r1)

-- | Symmetric to rotateR
rotateL     :: (Ord r, Fractional r)
                     => VertexID -> VertexID -> Vertex -> Merge p r (Vertex, Bool)
rotateL l r l1 | trace ("ROTL: " ++ show (l,r,l1))               False = undefined
rotateL l r l1 = focus' l1 `isRightOf` (r, l) >>= \case
                   True  -> (,False) <$> rotateL' l r l1 (succ' l1)
                   False -> trace "WRONG?" $ pure (l1,True)

-- | The code that does the actual rotating. Symmetric to rotateR'
rotateL'             :: (Ord r, Fractional r)
                     => VertexID -> VertexID -> Vertex -> Vertex -> Merge p r Vertex
rotateL' l r l1' l2' = go l1' l2'
  where
    go l1 l2 | trace ("GL " ++ show (l,r,l1,l2)) False = undefined
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
                      in not . maybe True ((k'^.core) `insideBall`) $ disk' h' i' j'
    disk' p q r = disk (p^.core) (q^.core) (r^.core)

-- | Inserts an edge into the right position.
insert     :: (Num r, Ord r) => VertexID -> VertexID -> Merge p r ()
insert u v = ask >>= modify . insert' u v

-- | Inserts an edge (and makes sure that the vertex is inserted in the
-- correct. pos in the adjacency lists)
insert'               :: (Num r, Ord r)
                      => VertexID -> VertexID -> Mapping p r -> Adj -> Adj
insert' u v _ ad | trace ("insert' on " ++ show (u,v,ad)) False = undefined
insert' u v (_,ptMap) ad = (\adj' -> trace ("ADJ: After Inserting " ++ show (u,v,adj')) adj')
                           .  IM.adjustWithKey (insert'' v) u . IM.adjustWithKey (insert'' u) v $ ad
  where
    -- inserts a into the adjacency list of b
    insert'' bi ai adjA = case C.toList adjA of
        []     -> C.singleton bi
        [ci]   -> C.fromList [ci,bi]
        (ci:_) -> let a = ptMap V.! ai
                      b = ptMap V.! bi
                  in fromJust . C.rotateTo ci . C.insertL bi .
                     Convex.rotateRWhile (\_ di -> trace "foo" $
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


lookup'     :: (Ord k, Show a, Show k) => M.Map k a -> k -> a
-- lookup' m x | traceShow (m,x) False = undefined
lookup' m x = fromJust $ M.lookup x m

size'              :: BinLeafTree Size a -> Size
size' (Leaf _)     = 1
size' (Node _ s _) = s

--------------------------------------------------------------------------------

myPoints :: NonEmpty.NonEmpty (Point 2 Rational :+ ())
myPoints = NonEmpty.fromList . map only $
           [ point2 1  3
           , point2 4  26
           , point2 5  17
           , point2 6  7
           -- , point2 12 16
           -- , point2 19 4
           -- , point2 20 0
           -- , point2 20 11
           -- , point2 23 23
           -- , point2 31 14
           -- , point2 33 5
           ]
