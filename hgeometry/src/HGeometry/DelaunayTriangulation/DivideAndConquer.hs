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
  -- , foo
  ) where

import           Control.Lens
import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.CircularList as CL
import           Data.Foldable1
import           Data.Function (on)
import qualified Data.IntMap.Strict as IM
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as M
import           Data.Maybe (fromJust, fromMaybe)
import qualified Data.Vector as V
import qualified Data.Vector.NonEmpty as NV
-- import           HGeometry.Algorithms.DivideAndConquer
import           HGeometry.Ball
import           HGeometry.Boundary
import qualified HGeometry.CircularList.Util as CU
import           HGeometry.ConvexHull.GrahamScan as GS
-- import qualified HGeometry.Cyclic
import           HGeometry.DelaunayTriangulation.Types
-- import qualified HGeometry.DelaunayTriangulation.Types as Naive
import           HGeometry.Ext
import           HGeometry.Foldable.Sort
import           HGeometry.LineSegment
import           HGeometry.Measured.Size
import           HGeometry.Point
import           HGeometry.Polygon.Class
import           HGeometry.Polygon.Convex
import qualified HGeometry.Polygon.Convex.Merge as Convex
import           HGeometry.Polygon.Simple.Class
-- import           HGeometry.Polygon.Simple.PossiblyDegenerate
import           HGeometry.Tree.Binary.Static

-- import           HGeometry.Number.Real.Rational
-- import           Debug.Trace

-------------------------------------------------------------------------------

-- type R = RealNumber 5


-- TODO: this could use a rewrite. we shouldn't need the mapping
-- and it would be nice to just use the divideAndConquer1With strategy


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
-- successor on the convex hull
--
-- Rotating Right <-> rotate clockwise

-- | Computes the delaunay triangulation of a set of points.
--
-- Running time: \(O(n \log^2 n)\)
-- (note: the extra \(\log n\) factor is since we use an IntMap in the implementation)
--
-- pre:
--      - the input is a *SET*, i.e. contains no duplicate points.
--      - no three colinear points
--      - no four cocircular points
delaunayTriangulation     :: (Foldable1 set, Point_ point 2 r, Ord point, Ord r, Num r
                             )
                          => set point -> Triangulation point
delaunayTriangulation pts = Triangulation vtxMap ptsV adjV
  where
    ptsV   = sort pts -- sort lexicographically. TODO: make this more explicit
    vtxMap = M.fromList . V.toList . V.imap (\i v -> (v,i)) $ ptsV

    tr     = asBalancedBinLeafTree $ NV.unsafeFromVector ptsV

    (adj,_) = delaunayTriangulation' tr (vtxMap,ptsV)
    adjV    = V.fromList . IM.elems $ adj


-- mergeConvex      :: PossiblyDegenerateSimplePolygon point (ConvexPolygon point)
--                  -> PossiblyDegenerateSimplePolygon point (ConvexPolygon point)
--                  -> PossiblyDegenerateSimplePolygon point (ConvexPolygon point)
-- mergeConvex l = \case
--   ActualPolygon rCH -> case l of
--     ActualPolygon lCH -> Convex.Merge lCH rCH
--     _                 -> foldr insertVertex rCH l
--   r                  -> case l of
--     ActualPolygon lCH -> foldr insertVertex lCH r
--     _                 -> GS.grahamScan (toNonEmpty l <> toNonEmpty l)


-- -- | Given a point outside the convex polygon, insert it into the polygon
-- insertVertex      :: point -> ConvexPolygon polygon -> ConvexPolygon point
-- insertVertex q pg = let




-- : pre: - Input points are sorted lexicographically
delaunayTriangulation' :: (Point_ point 2 r, Ord point, Ord r, Num r)
                       => BinLeafTree (Count point) point
                       -> Mapping point
                       -> (Adj, ConvexPolygon (point :+ VertexID))
delaunayTriangulation' pts mapping'@(vtxMap,_) = case pts of
  Leaf p        -> let i = lookup' vtxMap p
                   in ( IM.singleton i CL.empty
                      , uncheckedFromCCWPoints . NonEmpty.singleton $ p :+ i
                      )
                        -- FIXME: this deifnes an invalid polygon..
  Node lt (Count size) rt
    | size <= 3 -> let pts'  = (\p -> p :+ lookup' vtxMap p) <$> pts
                       ch    = GS.convexHull pts'
                   in (fromHull mapping' ch, ch)
    | otherwise -> let (ld,lch)       = delaunayTriangulation' lt mapping'
                       (rd,rch)       = delaunayTriangulation' rt mapping'
                       (ch, bt, ut)   = Convex.merge lch rch
                   in ( merge ld rd (view core <$> bt) (view core <$> ut) mapping' (firsts ch)
                      , ch
                      )

--------------------------------------------------------------------------------
-- * Implementation

-- | Mapping that says for each vtx in the convex hull what the first entry in
-- the adj. list should be.
firsts    :: (Point_ point 2 r)
          => ConvexPolygon (point :+ VertexID) -> IM.IntMap VertexID
firsts pg = IM.fromList . map (bimap (^.extra) (^.extra)) $ pg^..outerBoundaryEdges

-- | Given a polygon; construct the adjacency list representation
-- pre: at least two elements
fromHull              :: (Point_ point 2 r, Ord point)
                      => Mapping point -> ConvexPolygon (point :+ VertexID) -> Adj
fromHull (vtxMap,_) p = let vs@(u:v:vs') = lookup' vtxMap . (^.core)
                                        <$> p^..vertices
                            es           = zipWith3 f vs (List.drop 1 vs ++ [u]) (vs' ++ [u,v])
                            f prv c nxt  = (c,CL.fromList . List.nub $ [prv, nxt])
                        in IM.fromList es


-- | Merge the two delaunay triangulations.
--
-- running time: \(O(n)\) (although we cheat a bit by using a IntMap)
merge                            :: (Point_ point 2 r, Ord point, Ord r, Num r)
                                 => Adj
                                 -> Adj
                                 -> ClosedLineSegment (point :+ VertexID)
                                 -- ^ lower tangent
                                 -> ClosedLineSegment (point:+ VertexID)
                                 -- ^ upper tangent
                                 -> Mapping point
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

type Merge point = StateT Adj (Reader (Mapping point, Firsts))

type Firsts = IM.IntMap VertexID

-- | Merges the two delaunay traingulations.
moveUp          :: (Point_ point 2 r, Ord r, Num r)
                => (VertexID,VertexID) -> VertexID -> VertexID -> Merge point ()
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
rotateR        :: (Point_ point 2 r, Ord r, Num r)
               => VertexID -> VertexID -> DTVertex -> Merge point (DTVertex, Bool)
rotateR l r r1 = focus' r1 `isLeftOf` (l, r) >>= \case
                   True  -> (,False) <$> rotateR' l r r1 (pred' r1)
                   False -> pure (r1,True)


-- | The code that does the actual rotating
rotateR'     :: (Point_ point 2 r, Ord r, Num r)
             => VertexID -> VertexID -> DTVertex -> DTVertex -> Merge point DTVertex
rotateR' l r = go
  where
    go r1 r2 = qTest l r r1 r2 >>= \case
                 True  -> pure r1
                 False -> do modify $ delete r (focus' r1)
                             go r2 (pred' r2)


-- | Symmetric to rotateR
rotateL        :: (Point_ point 2 r, Ord r, Num r)
               => VertexID -> VertexID -> DTVertex -> Merge point (DTVertex, Bool)
rotateL l r l1 = focus' l1 `isRightOf` (r, l) >>= \case
                   True  -> (,False) <$> rotateL' l r l1 (succ' l1)
                   False -> pure (l1,True)

-- | The code that does the actual rotating. Symmetric to rotateR'
rotateL'     :: (Point_ point 2 r, Ord r, Num r)
             => VertexID -> VertexID -> DTVertex -> DTVertex -> Merge point DTVertex
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
qTest         :: (Point_ point 2 r, Ord r, Num r)
              => VertexID -> VertexID -> DTVertex -> DTVertex -> Merge point Bool
qTest h i j k = asks (withPtMap . snd . fst)
  where
    withPtMap ptMap = let h' = ptMap V.! h
                          i' = ptMap V.! i
                          j' = ptMap V.! focus' j
                          k' = ptMap V.! focus' k
                      in not . maybe True (k' `insideBall`) $ diskFromPoints h' i' j'
    insideBall q b = q `inBall` b == Inside
    -- should be strictly inside the ball


-- | Inserts an edge into the right position.
insert     :: (Point_ point 2 r, Num r, Ord r) => VertexID -> VertexID -> Merge point ()
insert u v = do
               (mapping',fsts) <- ask
               modify $ insert' u v mapping'
               rotateToFirst u fsts
               rotateToFirst v fsts


-- | make sure that the first vtx in the adj list of v is its predecessor on the CH
rotateToFirst        :: VertexID -> Firsts -> Merge point ()
rotateToFirst v fsts = modify $ IM.adjust f v
  where
    mfst   = IM.lookup v fsts
    f  cl  = fromMaybe cl $ mfst >>= flip CL.rotateTo cl


-- | Inserts an edge (and makes sure that the vertex is inserted in the
-- correct. pos in the adjacency lists)
insert'               :: (Point_ point 2 r, Num r, Ord r)
                      => VertexID -> VertexID -> Mapping point -> Adj -> Adj
insert' u v (_,ptMap) = IM.adjustWithKey (insert'' v) u
                      . IM.adjustWithKey (insert'' u) v
  where
    -- inserts b into the adjacency list of a
    insert'' bi ai = CU.insertOrdBy (cmp (ptMap V.! ai) `on` (ptMap V.!)) bi
    cmp c p q = cwCmpAround c p q <> cmpByDistanceTo c p q


-- | Deletes an edge
delete     :: VertexID -> VertexID -> Adj -> Adj
delete u v = IM.adjust (delete' v) u . IM.adjust (delete' u) v
  where
    delete' x = CL.filterL (/= x) -- should we rotate left or right if it is the focus?


-- | Lifted version of Convex.IsLeftOf
isLeftOf           :: (Point_ point 2 r, Ord r, Num r)
                   => VertexID -> (VertexID, VertexID) -> Merge point Bool
p `isLeftOf` (l,r) = asks (withPtMap . snd . fst)
  where
    withPtMap ptMap = (ptMap V.! p) `isLeftOf'` (ptMap V.! l, ptMap V.! r)
    a `isLeftOf'` (b,c) = ccw b c a == CCW

-- | Lifted version of Convex.IsRightOf
isRightOf           :: (Point_ point 2 r, Ord r, Num r)
                    => VertexID -> (VertexID, VertexID) -> Merge point Bool
p `isRightOf` (l,r) = asks (withPtMap . snd . fst)
  where
    withPtMap ptMap = (ptMap V.! p) `isRightOf'` (ptMap V.! l, ptMap V.! r)
    a `isRightOf'` (b,c) = ccw b c a == CW

--------------------------------------------------------------------------------
-- * Some Helper functions


lookup'     :: Ord k => M.Map k a -> k -> a
lookup' m x = fromJust $ M.lookup x m


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

lookup'' :: Int -> IM.IntMap a -> a
lookup'' k m = m IM.! k

--------------------------------------------------------------------------------



-- _foo = do mapM print (asBalancedBinLeafTree myPoints)
--          print $ delaunayTr myPoints

--          -- print "dummy"
--          -- let f p = (uncheckedFromCCWPoints . NonEmpty.singleton $ p ) :: ConvexPolygon (Point 2 R)
--          -- print $ Convex.merge (f $ Point2 1  3) (f $ Point2 4  26)
--          -- print "dummy"

--          -- print "================================================================================"

--          -- let g s = (uncheckedFromCCWPoints . NonEmpty.fromList $ s) :: ConvexPolygon (Point 2 R)
--          -- print $ mergeX (g [Point2 4  26, Point2 1 3]) (g [Point2 6 7, Point2 5 17])


--          -- print "============"

--          -- print $ chByMerge myPoints
--          -- print $ delaunayTr myPoints

-- myPoints :: NonEmpty.NonEmpty (Point 2 R)
-- myPoints = NonEmpty.fromList $
--             [ Point2 64  736
--             , Point2 96 688
--             , Point2 128 752
--             , Point2 160 704
--             , Point2 128 672
--             , Point2 64 656
--             , Point2 192 736
--             -- , Point2 208 704
--             -- , Point2 192 672
--             ]
