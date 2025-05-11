--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.DelaunayTriangulation.Naive
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--------------------------------------------------------------------------------
module HGeometry.DelaunayTriangulation.Naive where

import           Control.Lens
import qualified Data.CircularList as C
import           Data.Foldable
import           Data.Foldable1
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import           HGeometry.Ball
import           HGeometry.Boundary
import           HGeometry.DelaunayTriangulation.Types
import           HGeometry.Ext
import           HGeometry.Foldable.Util
import           HGeometry.Point

--------------------------------------------------------------------------------

-- | Naive \( O(n^4) \) time implementation of the delaunay triangulation. Simply
-- tries each triple (p,q,r) and tests if it is delaunay, i.e. if there are no
-- other points in the circle defined by p, q, and r.
--
-- pre: the input is a *SET*, i.e. contains no duplicate points.
delaunayTriangulation     :: ( Foldable1 set, Point_ point 2 r, Ord point, Ord r, Num r)
                          => set point -> Triangulation point
delaunayTriangulation pts = Triangulation ptIds ptsV adjV
  where
    ptsV   = fromFoldable pts
    ptIds  = Map.fromList $ zip (toList pts) [0..]
    -- construct the list of faces/triangles in the delaunay triangulation
    fs = [ (p,q,r)
         | p <- [0..n], q <- [p..n], r <- [q..n], isDelaunay (ptIds,ptsV) p q r
         ]
    edges  = foldMap (\(p,q,r) -> Set.fromList [(p,q), (q,r), (p,r)]) fs
    adjV   = toAdjLists (ptIds,ptsV) edges
    n      = V.length ptsV - 1

-- | Given a list of edges, as vertexId pairs, construct a vector with the
-- adjacency lists, each in CW sorted order.
toAdjLists               :: (Point_ point 2 r, Num r, Ord r)
                         => Mapping point -> Set.Set (VertexID,VertexID)
                         -> V.Vector (C.CList VertexID)
toAdjLists m@(_,ptsV) es = V.imap toCList $ V.create $ do
    v <- MV.replicate (V.length ptsV) []
    for_ es $ \(i,j) -> do
      addAt v i j
      addAt v j i
    pure v
  where
    updateAt v i f = MV.read v i >>= \x -> MV.write v i (f x)
    addAt    v i j = updateAt v i (j:)

    -- convert to a CList, sorted in CCW order around point u
    toCList u = C.fromList . sortAroundMapping m u

-- | Given a particular point u and a list of points vs, sort the points vs in
-- CW order around u.
-- running time: \( O(m log m) \), where m=|vs| is the number of vertices to sort.
sortAroundMapping               :: (Point_ point 2 r, Num r, Ord r)
                                => Mapping point -> VertexID -> [VertexID] -> [VertexID]
sortAroundMapping (_,ptsV) u vs = reverse . map (^.extra) $ sortAround (f u) (map f vs)
  where
    f v = (ptsV V.! v) :+ v



-- | \( O(n) \) Test if the given three points form a triangle in the delaunay triangulation.
isDelaunay                :: ( Point_ point 2 r, Num r, Ord r)
                          => Mapping point -> VertexID -> VertexID -> VertexID -> Bool
isDelaunay (_,ptsV) a b c = case diskFromPoints (pt a) (pt b) (pt c) of
    Nothing   -> False -- if the points are colinear, we interpret this as: all
                       -- pts in the plane are in the circle.
    Just disk -> not $ any (\q -> q `inBall` disk == Inside)
                 [pt i | i <- [0..(V.length ptsV - 1)], i /= a, i /= b, i /= c]
   where
     pt i = ptsV V.! i
