--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.DelaunayTriangulation.DivideAndConquer
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--------------------------------------------------------------------------------
module HGeometry.DelaunayTriangulation.DivideAndConquer
  (
    -- * Divide & Conqueror Delaunay Triangulation
    delaunayTriangulation
  ) where

import           Control.Lens
import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.CircularList as CL
import qualified Data.Foldable as F
import           Data.Function (on)
import qualified Data.IntMap.Strict as IM
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as M
import           Data.Maybe (fromJust, fromMaybe)
import qualified Data.Vector as V
import           HGeometry.Algorithms.DivideAndConquer
import           HGeometry.Ball
import qualified HGeometry.CircularList.Util as CU
import           HGeometry.ConvexHull.GrahamScan as GS
import qualified HGeometry.Cyclic
import           HGeometry.DelaunayTriangulation.Types
import           HGeometry.Ext
import           HGeometry.LineSegment
import           HGeometry.Measured.Size
import           HGeometry.Point
import           HGeometry.Polygon.Convex
import qualified HGeometry.Polygon.Convex.Merge as Convex
import           HGeometry.Tree.Binary.Static


-------------------------------------------------------------------------------
-- * Divide & Conqueror Delaunay Triangulation
--
-- Implementation of the Divide & Conqueror algorithm as described in:
--
-- Two Algorithms for Constructing a Delaunay Triangulation
-- Lee and Schachter
-- International Journal of Computer and Information Sciences, Vol 9, No. 3, 1980
--
-- We store all adjacency lists in clockwise order
--
-- : If v on the convex hull, then its first entry in the adj. lists is its CCW
-- successor (i.e. its predecessor) on the convex hull
--
-- Rotating Right <-> rotate clockwise

-- | Computes the delaunay triangulation of a set of points.
--
-- Running time: \(O(n \log n)\)
-- (note: We use an IntMap in the implementation. So maybe actually \(O(n \log^2 n)\))
--
-- pre: the input is a *SET*, i.e. contains no duplicate points. (If the
-- input does contain duplicate points, the implementation throws them away)
delaunayTriangulation      :: (Ord r, Fractional r)
                           => NonEmpty.NonEmpty (Point 2 r :+ p) -> Triangulation p r
delaunayTriangulation pts' = Triangulation vtxMap ptsV adjV
  where
    pts    = nub' . NonEmpty.sortBy (compare `on` (^.core)) $ pts'
    ptsV   = V.fromList . F.toList $ pts
    vtxMap = M.fromList $ zip (map (^.core) . V.toList $ ptsV) [0..]

    tr     = _unElem <$> asBalancedBinLeafTree pts

    (adj,_) = delaunayTriangulation' tr (vtxMap,ptsV)
    adjV    = V.fromList . IM.elems $ adj



-- : pre: - Input points are sorted lexicographically
delaunayTriangulation' :: (Ord r, Fractional r)
                       => BinLeafTree (Count ) (Point 2 r :+ p)
                       -> Mapping p r
                       -> (Adj, ConvexPolygon (Point 2 r :+ (p :+ VertexID)) r)
delaunayTriangulation' pts mapping'@(vtxMap,_)
  | size' pts == 1 = let (Leaf p) = pts
                         i        = lookup' vtxMap (p^.core)
                     in (IM.singleton i CL.empty, uncheckedFromPoints [withID p i])
  | size' pts <= 3 = let pts'  = NonEmpty.fromList
                               . map (\p -> withID p (lookup' vtxMap (p^.core)))
                               . F.toList $ pts
                         ch    = GS.convexHull pts'
                     in (fromHull mapping' ch, ch)
  | otherwise      = let (Node lt _ rt) = pts
                         (ld,lch)       = delaunayTriangulation' lt mapping'
                         (rd,rch)       = delaunayTriangulation' rt mapping'
                         (ch, bt, ut)   = Convex.merge lch rch
                     in (merge ld rd bt ut mapping' (firsts ch), ch)

--------------------------------------------------------------------------------
-- * Implementation



-- | Mapping that says for each vtx in the convex hull what the first entry in
-- the adj. list should be. The input polygon is given in Clockwise order
firsts :: ConvexPolygon (Point 2 r :+ (p :+ VertexID)) r -> IM.IntMap VertexID
firsts = IM.fromList . map (\s -> (s^.end.extra.extra, s^.start.extra.extra))
       . F.toList . outerBoundaryEdges . _simplePolygon


-- | Given a polygon; construct the adjacency list representation
-- pre: at least two elements
fromHull              :: Ord r => Mapping p r -> ConvexPolygon (Point 2 r :+ (p :+ q)) r -> Adj
fromHull (vtxMap,_) p = let vs@(u:v:vs') = map (lookup' vtxMap . (^.core))
                                         $ pg^..vertices
                                         -- . F.toList . CV.rightElements
                                         -- $ p^.simplePolygon.outerBoundaryVector

                            es           = zipWith3 f vs (tail vs ++ [u]) (vs' ++ [u,v])
                            f prv c nxt  = (c,CL.fromList . List.nub $ [prv, nxt])
                        in IM.fromList es


-- | Merge the two delaunay triangulations.
--
-- running time: \(O(n)\) (although we cheat a bit by using a IntMap)
merge                            :: (Ord r, Fractional r)
                                 => Adj
                                 -> Adj
                                 -> LineSegment (Point 2 r :+ (p :+ VertexID))
                                 -- ^ lower tangent
                                 -> LineSegment 2 (Point 2 r :+ (p :+ VertexID))
                                 -- ^ upper tangent
                                 -> Mapping p r
                                 -> Firsts
                                 -> Adj
merge ld rd bt ut mapping'@(vtxMap,_) fsts =
    flip runReader (mapping', fsts) . flip execStateT adj $ moveUp (tl,tr) l r
  where
    l   = lookup' vtxMap (bt^.start.core)
    r   = lookup' vtxMap (bt^.end.core)
    tl  = lookup' vtxMap (ut^.start.core)
    tr  = lookup' vtxMap (ut^.end.core)
    adj = ld `IM.union` rd

type Merge p r = StateT Adj (Reader (Mapping p r, Firsts))

type Firsts = IM.IntMap VertexID

-- | Merges the two delaunay traingulations.
moveUp          :: (Ord r, Fractional r)
                => (VertexID,VertexID) -> VertexID -> VertexID -> Merge p r ()
moveUp ut l r
  | (l,r) == ut = insert l r
  | otherwise   = do
                     insert l r
                     -- Get the neighbours of r and l along the convex hull
                     r1 <- gets (pred' . rotateTo l . lookup'' r)
                     l1 <- gets (succ' . rotateTo r . lookup'' l)

                     (r1',a) <- rotateR l r r1
                     (l1',b) <- rotateL l r l1
                     c       <- qTest l r r1' l1'
                     let (l',r') = case (a,b,c) of
                                     (True,_,_)          -> (focus' l1', r)
                                     (False,True,_)      -> (l,          focus' r1')
                                     (False,False,True)  -> (l,          focus' r1')
                                     (False,False,False) -> (focus' l1', r)
                     moveUp ut l' r'


-- | \'rotates\' around r and removes all neighbours of r that violate the
-- delaunay condition. Returns the first vertex (as a Neighbour of r) that
-- should remain in the Delaunay Triangulation, as well as a boolean A that
-- helps deciding if we merge up by rotating left or rotating right (See
-- description in the paper for more info)
rotateR        :: (Ord r, Fractional r)
               => VertexID -> VertexID -> DTVertex -> Merge p r (DTVertex, Bool)
rotateR l r r1 = focus' r1 `isLeftOf` (l, r) >>= \case
                   True  -> (,False) <$> rotateR' l r r1 (pred' r1)
                   False -> pure (r1,True)


-- | The code that does the actual rotating
rotateR'     :: (Ord r, Fractional r)
             => VertexID -> VertexID -> DTVertex -> DTVertex -> Merge p r DTVertex
rotateR' l r = go
  where
    go r1 r2 = qTest l r r1 r2 >>= \case
                 True  -> pure r1
                 False -> do modify $ delete r (focus' r1)
                             go r2 (pred' r2)


-- | Symmetric to rotateR
rotateL     :: (Ord r, Fractional r)
                     => VertexID -> VertexID -> DTVertex -> Merge p r (DTVertex, Bool)
rotateL l r l1 = focus' l1 `isRightOf` (r, l) >>= \case
                   True  -> (,False) <$> rotateL' l r l1 (succ' l1)
                   False -> pure (l1,True)

-- | The code that does the actual rotating. Symmetric to rotateR'
rotateL'     :: (Ord r, Fractional r)
             => VertexID -> VertexID -> DTVertex -> DTVertex -> Merge p r DTVertex
rotateL' l r = go
  where
    go l1 l2 = qTest l r l1 l2 >>= \case
                 True  -> pure l1
                 False -> do modify $ delete l (focus' l1)
                             go l2 (succ' l2)

--------------------------------------------------------------------------------
-- * Primitives used by the Algorithm

-- | returns True if the forth point (vertex) does not lie in the disk defined
-- by the first three points.
qTest         :: (Ord r, Fractional r)
              => VertexID -> VertexID -> DTVertex -> DTVertex -> Merge p r Bool
qTest h i j k = asks (withPtMap . snd . fst)
  where
    withPtMap ptMap = let h' = ptMap V.! h
                          i' = ptMap V.! i
                          j' = ptMap V.! focus' j
                          k' = ptMap V.! focus' k
                      in not . maybe True ((k'^.core) `insideBall`) $ disk' h' i' j'
    disk' p q r = disk (p^.core) (q^.core) (r^.core)

-- | Inserts an edge into the right position.
insert     :: (Num r, Ord r) => VertexID -> VertexID -> Merge p r ()
insert u v = do
               (mapping',fsts) <- ask
               modify $ insert' u v mapping'
               rotateToFirst u fsts
               rotateToFirst v fsts


-- | make sure that the first vtx in the adj list of v is its predecessor on the CH
rotateToFirst        :: VertexID -> Firsts -> Merge p r ()
rotateToFirst v fsts = modify $ IM.adjust f v
  where
    mfst   = IM.lookup v fsts
    f  cl  = fromMaybe cl $ mfst >>= flip CL.rotateTo cl


-- | Inserts an edge (and makes sure that the vertex is inserted in the
-- correct. pos in the adjacency lists)
insert'               :: (Num r, Ord r)
                      => VertexID -> VertexID -> Mapping p r -> Adj -> Adj
insert' u v (_,ptMap) = IM.adjustWithKey (insert'' v) u
                      . IM.adjustWithKey (insert'' u) v
  where
    -- inserts b into the adjacency list of a
    insert'' bi ai = CU.insertOrdBy (cmp (ptMap V.! ai) `on` (ptMap V.!)) bi
    cmp c p q = cwCmpAround' c p q <> cmpByDistanceTo' c p q


-- | Deletes an edge
delete     :: VertexID -> VertexID -> Adj -> Adj
delete u v = IM.adjust (delete' v) u . IM.adjust (delete' u) v
  where
    delete' x = CL.filterL (/= x) -- should we rotate left or right if it is the focus?


-- | Lifted version of Convex.IsLeftOf
isLeftOf           :: (Ord r, Num r)
                   => VertexID -> (VertexID, VertexID) -> Merge p r Bool
p `isLeftOf` (l,r) = asks (withPtMap . snd . fst)
  where
    withPtMap ptMap = (ptMap V.! p) `isLeftOf'` (ptMap V.! l, ptMap V.! r)
    a `isLeftOf'` (b,c) = ccw b c a == CCW

-- | Lifted version of Convex.IsRightOf
isRightOf           :: (Ord r, Num r)
                    => VertexID -> (VertexID, VertexID) -> Merge p r Bool
p `isRightOf` (l,r) = asks (withPtMap . snd . fst)
  where
    withPtMap ptMap = (ptMap V.! p) `isRightOf'` (ptMap V.! l, ptMap V.! r)
    a `isRightOf'` (b,c) = ccw b c a == CW

--------------------------------------------------------------------------------
-- * Some Helper functions


lookup'     :: Ord k => M.Map k a -> k -> a
lookup' m x = fromJust $ M.lookup x m

size'              :: BinLeafTree (Sized a) a -> Sized a
size' (Leaf x)     = 1
size' (Node _ s _) = s

-- | an \'unsafe\' version of rotateTo that assumes the element to rotate to
-- occurs in the list.
rotateTo   :: Eq a => a -> CL.CList a -> CL.CList a
rotateTo x = fromJust . CL.rotateTo x

-- | Adjacency lists are stored in clockwise order, so pred means rotate right
pred' :: CL.CList a -> CL.CList a
pred' = CL.rotR

-- | Adjacency lists are stored in clockwise order, so pred and succ rotate left
succ' :: CL.CList a -> CL.CList a
succ' = CL.rotL

-- | Return the focus of the CList, throwing an exception if the list is empty.
focus' :: CL.CList a -> a
focus' = fromJust . CL.focus

-- | Removes duplicates from a sorted list
nub' :: Eq a => NonEmpty.NonEmpty (a :+ b) -> NonEmpty.NonEmpty (a :+ b)
nub' = fmap NonEmpty.head . NonEmpty.groupBy1 ((==) `on` (^.core))


withID     :: c :+ e -> e' -> c :+ (e :+ e')
withID p i = p&extra %~ (:+i)

lookup'' :: Int -> IM.IntMap a -> a
lookup'' k m = m IM.! k
