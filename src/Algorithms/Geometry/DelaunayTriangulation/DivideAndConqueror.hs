{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
module Algorithms.Geometry.DelaunayTriangulation.DivideAndConqueror where

import Control.Applicative
import Algorithms.Geometry.DelaunayTriangulation.Types
import Algorithms.Geometry.ConvexHull.GrahamScan as GS
import Control.Monad.State
import Control.Monad.Reader
import Control.Lens
import Data.Function(on)
import qualified Data.Foldable as F
import Data.Maybe(fromJust, fromMaybe)

import qualified Data.List as L
import qualified Data.Vector as V
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as M
import qualified Data.IntMap.Strict as IM
import qualified Data.CircularList as C
import qualified Data.CircularList.Util as CU
import Data.Geometry.Polygon.Convex(ConvexPolygon)
import qualified Data.Geometry.Polygon.Convex as Convex
import Data.Geometry.Polygon.Convex(focus')
import Data.Ext
import Data.Geometry
import Data.Geometry.Interval
import Data.Geometry.Polygon
import Data.Geometry.Ball(disk, insideBall)

import Data.BinaryTree

--------------------------------------------------------------------------------

import Data.Geometry.Ipe
import Debug.Trace
import qualified Algorithms.Geometry.DelaunayTriangulation.Naive as Naive

input :: [Point 2 Rational]
input = [ point2 64  736
        , point2 96 688
        , point2 128 752
        , point2 160 704
        , point2 128 672
        , point2 64 656
        , point2 192 736
        , point2 208 704
        , point2 192 672
        ]


testz = delaunayTriangulation . NonEmpty.fromList . map ext $ input

main = do
         Right (page :: IpePage Rational) <- readSinglePageFile "/Users/frank/tmp/dt1.ipe"
         let syms = page^..content.traverse._IpeUse
             pts  = map (&extra .~ ()) $ map (\s -> s&core %~ (^.symbolPoint)) syms
             dt  = delaunayTriangulation $ NonEmpty.fromList pts
         pure dt
         -- -- mapM_ (\(p :+ _) -> print p) pts
         -- print dt
         -- printAsIpeSelection $ asIpe drawTriangulation dt




-------------------------------------------------------------------------------
-- Implementation of the Divide & Conqueror algorithm as described in:
--
-- Two Algorithms for Constructing a Delaunay Triangulation
-- Lee and Schachter
-- International Journal of Computer and Information Sciences, Vol 9, No. 3, 1980

-- We store all adjacency lists in clockwise order

-- : If v on the convex hull, then its first entry in the adj. lists is its CCW
-- successor (i.e. its predecessor) on the convex hull

-- | Rotating Right <-> rotate clockwise







data TriangRes p r = TriangRes { _triang     :: Triangulation p r
                               , _convexHull :: ConvexPolygon p r
                               }


-- | Computes the delaunay triangulation of a set of points.
--
-- Running time: O(n log n)
--
-- pre: the input is a *SET*, i.e. contains no duplicate points. (If the
-- input does contain duplicate points, the implementation throws them away)
delaunayTriangulation      :: (Ord r, Fractional r,       Show r, Show p)
                           => NonEmpty.NonEmpty (Point 2 r :+ p) -> Triangulation p r
delaunayTriangulation pts' = Triangulation vtxMap ptsV adjV
  where
    pts    = nub' . NonEmpty.sortBy (compare `on` (^.core)) $ pts'
    ptsV   = V.fromList . F.toList $ pts
    vtxMap = M.fromList $ zip (map (^.core) . V.toList $ ptsV) [0..]

    tr     = _unElem <$> asBalancedBinLeafTree pts

    (adj,_) = delaunayTriangulation' tr (vtxMap,ptsV)
    adjV    = V.fromList . IM.elems $ adj

-- | Removes duplicates from a sorted list
nub' :: Eq a => NonEmpty.NonEmpty (a :+ b) -> NonEmpty.NonEmpty (a :+ b)
nub' = fmap NonEmpty.head . NonEmpty.groupBy1 ((==) `on` (^.core))

type Adj = IM.IntMap (C.CList VertexID)


withID      :: c :+ e -> e' -> c :+ (e :+ e')
withID p pi = p&extra %~ (:+pi)

-- : pre: - Input points are sorted lexicographically
delaunayTriangulation'   :: (Ord r, Fractional r,      Show r, Show p)
                         => BinLeafTree Size (Point 2 r :+ p)
                         -> Mapping p r
                         -> (Adj, ConvexPolygon (p :+ VertexID) r)
delaunayTriangulation' pts mapping@(vtxMap,_)
  | size' pts == 1 = let (Leaf p) = pts
                         pi       = lookup' vtxMap (p^.core)
                     in (IM.singleton pi C.empty, fromPoints [withID p pi])
  | size' pts <= 3 = let pts'            = NonEmpty.fromList
                                         . map (\p -> withID p (lookup' vtxMap (p^.core)))
                                         . F.toList $ pts
                         (ConvexHull ch) = GS.convexHull pts'
                     in (fromHull mapping ch, ch)
  | otherwise      = let (Node lt _ rt) = pts
                         (ld,lch)       = delaunayTriangulation' lt mapping
                         (rd,rch)       = delaunayTriangulation' rt mapping
                         (ch, bt, ut)   = Convex.merge lch rch
                     in --trace ("HULLS: " ++ show (lch,rch, bt)) $
                        (merge ld rd bt ut mapping (firsts ch), ch)


-- | Mapping that says for each vtx in the convex hull what the first entry in
-- the adj. list should be. The input polygon is given in Clockwise order
firsts    :: SimplePolygon (p :+ VertexID) r -> IM.IntMap VertexID
firsts = IM.fromList . map (\s -> (s^.end.extra.extra, s^.start.extra.extra))
       . C.toList . outerBoundaryEdges



-- | Given a polygon; construct the adjacency list representation
-- pre: at least two elements

fromHull              :: Ord r => Mapping p r -> SimplePolygon (p :+ q) r -> Adj
fromHull (vtxMap,_) p = let vs@(u:v:vs') = map (lookup' vtxMap . (^.core))
                                         . C.rightElements $ p^.outerBoundary
                            es           = zipWith3 f vs (tail vs ++ [u]) (vs' ++ [u,v])
                            f prv c nxt  = (c,C.fromList . L.nub $ [prv, nxt])
                        in IM.fromList es


merge           :: (Ord r, Fractional r,      Show r, Show p)
                   => Adj
                -> Adj
                -> LineSegment 2 (p :+ VertexID) r
                -> LineSegment 2 (p :+ VertexID) r
                -> Mapping p r
                -> Firsts
                -> Adj
merge ld rd bt ut mapping@(vtxMap,_) fsts
  -- | trace ("Merge " ++ show (ld,rd,bt,ut)) False = undefined
  | otherwise                        = let l   = lookup' vtxMap (bt^.start.core)
                                           r   = lookup' vtxMap (bt^.end.core)
                                           tl  = lookup' vtxMap (ut^.start.core)
                                           tr  = lookup' vtxMap (ut^.end.core)
                                           adj = ld `IM.union` rd
                                       in   flip runReader (mapping, fsts)
                                          . flip execStateT adj $ moveUp (tl,tr) l r

type Firsts = IM.IntMap VertexID

type Merge p r = StateT Adj (Reader (Mapping p r, Firsts))


lookup'' :: Int -> IM.IntMap a -> a
-- lookup'' k m | traceShow (m,k) False = undefined
lookup'' k m = fromJust . IM.lookup k $ m

moveUp :: (Ord r, Fractional r,      Show r, Show p)
                     => (VertexID,VertexID) -> VertexID -> VertexID -> Merge p r ()
moveUp ut l r
  | (l,r) == ut = insert l r
  | otherwise   = do
                     -- trace ("============ STARTING ITERATION WITH " ++ show (l,r)) (pure ())
                     -- trace ("INSERTING " ++ show (l,r)) (pure ())
                     insert l r
                     adj <- get
                     -- trace ("Current adj:" ++ show adj) (pure ())
                     -- Get the neighbours of r and l along the convex hull
                     r1 <- pred' . rotateTo l . lookup'' r <$> get
                     l1 <- succ' . rotateTo r . lookup'' l <$> get

                     -- trace ("R1, L1:" ++ show (r1,l1)) (pure ())
                     (r1',a) <- rotateR l r r1
                     -- trace ("RotateR result: " ++ show (r1',a)) (pure ())
                     (l1',b) <- rotateL l r l1
                     -- trace ("RotateL result: " ++ show (l1',b)) (pure ())
                     c       <- qTest l r r1' l1'
                     -- trace ("adj after rotating:" ++ show adj) (pure ())
                     -- trace ("QTest result:" ++ show c) (pure ())
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
-- rotateR l r r1 | trace ("rotR " ++ show (l,r,r1))               False = undefined
rotateR l r r1 = focus' r1 `isLeftOf` (l, r) >>= \case
                   True  -> (,False) <$> rotateR' l r r1 (pred' r1)
                   False -> pure (r1,True)

-- | The code that does the actual rotating
rotateR'             :: (Ord r, Fractional r)
                     => VertexID -> VertexID -> Vertex -> Vertex -> Merge p r Vertex
-- rotateR' l r r1' r2' | trace ("RR'" ++ show (l,r,r1',r2')) False = undefined
rotateR' l r r1' r2' = go r1' r2'
  where
    -- go r1 r2 | trace ("GR" ++ show (r1,r2)) False = undefined
    go r1 r2 = qTest l r r1 r2 >>= \case
                 True  -> pure r1
                 False -> do modify $ delete r (focus' r1)
                             go r2 (pred' r2)



-- | Symmetric to rotateR
rotateL     :: (Ord r, Fractional r)
                     => VertexID -> VertexID -> Vertex -> Merge p r (Vertex, Bool)
-- rotateL l r l1 | trace ("ROTL: " ++ show (l,r,l1))               False = undefined
rotateL l r l1 = focus' l1 `isRightOf` (r, l) >>= \case
                   True  -> (,False) <$> rotateL' l r l1 (succ' l1)
                   False -> pure (l1,True)

-- | The code that does the actual rotating. Symmetric to rotateR'
rotateL'             :: (Ord r, Fractional r)
                     => VertexID -> VertexID -> Vertex -> Vertex -> Merge p r Vertex
rotateL' l r l1' l2' = go l1' l2'
  where
    -- go l1 l2 | trace ("GL " ++ show (l,r,l1,l2)) False = undefined
    go l1 l2 = qTest l r l1 l2 >>= \case
                 True  -> pure l1
                 False -> do modify $ delete l (focus' l1)
                             go l2 (succ' l2)



--------------------------------------------------------------------------------
-- * Primitives used by the Algorithm

-- | returns True if the forth point (vertex) does not lie in the disk defined
-- by the first three points.
qTest         :: (Ord r, Fractional r)
              => VertexID -> VertexID -> Vertex -> Vertex -> Merge p r Bool
qTest h i j k = withPtMap . snd . fst <$> ask
  where
    withPtMap ptMap = let h' = ptMap V.! h
                          i' = ptMap V.! i
                          j' = ptMap V.! (focus' j)
                          k' = ptMap V.! (focus' k)
                      in not . maybe True ((k'^.core) `insideBall`) $ disk' h' i' j'
    disk' p q r = disk (p^.core) (q^.core) (r^.core)

-- | Inserts an edge into the right position.
insert     :: (Num r, Ord r) => VertexID -> VertexID -> Merge p r ()
insert u v = do
               (mapping',fsts) <- ask
               modify $ insert' u v mapping'
               rotateToFirst u fsts
               rotateToFirst v fsts
               -- adj <- get
               -- trace ("ADJ: After Insert " ++ show (u,v,adj)) (pure ())


-- | make sure that the first vtx in the adj list of v is its predecessor on the CH
rotateToFirst        :: VertexID -> Firsts -> Merge p r ()
rotateToFirst v fsts = modify $ IM.adjust f v
  where
    mfst   = IM.lookup v fsts
    f  cl  = fromMaybe cl $ mfst >>= flip C.rotateTo cl




-- | Inserts an edge (and makes sure that the vertex is inserted in the
-- correct. pos in the adjacency lists)
insert'               :: (Num r, Ord r)
                      => VertexID -> VertexID -> Mapping p r -> Adj -> Adj
insert' u v (_,ptMap) = IM.adjustWithKey (insert'' v) u
                      . IM.adjustWithKey (insert'' u) v
  where
    -- inserts b into the adjacency list of a
    insert'' bi ai = CU.insertOrdBy (cwCmpAround (ptMap V.! ai) `on` (ptMap V.!)) bi


-- | Deletes an edge
delete     :: VertexID -> VertexID -> Adj -> Adj
delete u v = IM.adjust (delete' v) u . IM.adjust (delete' u) v
  where
    delete' x = C.filterL (/= x) -- should we rotate left or right if it is the focus?




-- | Lifted version of Convex.IsLeftOf
isLeftOf           :: (Ord r, Num r)
                   => VertexID -> (VertexID, VertexID) -> Merge p r Bool
p `isLeftOf` (l,r) = withPtMap . snd . fst <$> ask
  where
    withPtMap ptMap = (ptMap V.! p) `Convex.isLeftOf` (ptMap V.! l, ptMap V.! r)

-- | Lifted version of Convex.IsRightOf
isRightOf :: (Ord r, Num r) => VertexID -> (VertexID, VertexID) -> Merge p r Bool
p `isRightOf` (l,r) = withPtMap . snd . fst <$> ask
  where
    withPtMap ptMap = (ptMap V.! p) `Convex.isRightOf` (ptMap V.! l, ptMap V.! r)

--------------------------------------------------------------------------------
-- * Some Helper functions


lookup'     :: Ord k => M.Map k a -> k -> a
lookup' m x = fromJust $ M.lookup x m

size'              :: BinLeafTree Size a -> Size
size' (Leaf _)     = 1
size' (Node _ s _) = s

-- | an 'unsafe' version of rotateTo that assumes the element to rotate to
-- occurs in the list.
rotateTo   :: Eq a => a -> C.CList a -> C.CList a
rotateTo x = fromJust . C.rotateTo x

-- | Adjacency lists are stored in clockwise order, so pred means rotate right
pred' :: C.CList a -> C.CList a
pred' = C.rotR

-- | Adjacency lists are stored in clockwise order, so pred and succ rotate left
succ' :: C.CList a -> C.CList a
succ' = C.rotL
