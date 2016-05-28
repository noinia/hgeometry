module Algorithms.Geometry.EuclideanMST.EuclideanMST where

import           Algorithms.Geometry.DelaunayTriangulation.DivideAndConqueror
import           Algorithms.Geometry.DelaunayTriangulation.Types
import           Algorithms.Graph.MST
import           Control.Lens
import           Data.Ext
import           Data.Geometry
import           Data.Geometry.Ipe
import qualified Data.List.NonEmpty as NonEmpty
import           Data.PlaneGraph
import           Data.Proxy
import           Data.Tree


--------------------------------------------------------------------------------

-- | Computes the Euclidean Minimum Spanning Tree. We compute the Delaunay
-- Triangulation (DT), and then extract the EMST. Hence, the same restrictions
-- apply as for the DT:
--
-- pre: the input is a *SET*, i.e. contains no duplicate points. (If the input
-- does contain duplicate points, the implementation throws them away)
--
-- running time: $O(n \log n)$
euclideanMST     :: (Ord r, Fractional r, Show r, Show p)
                 => NonEmpty.NonEmpty (Point 2 r :+ p) -> Tree (Point 2 r :+ p)
euclideanMST pts = (\v -> g^.vDataOf v) <$> t
  where
    -- since we care only about the relative order of the edges we can use the
    -- squared Euclidean distance rather than the Euclidean distance, thus
    -- avoiding the Floating constraint
    g = withEdgeDistances squaredEuclideanDist . toPlaneGraph (Proxy :: Proxy MSTW)
      . delaunayTriangulation $ pts
    t = mst g


data MSTW


drawTree' :: IpeOut (Tree (Point 2 r :+ p)) (IpeObject r)
drawTree' = IpeOut $
  asIpeGroup . map (asIpeObject' mempty . uncurry ClosedLineSegment) . treeEdges


treeEdges              :: Tree a -> [(a,a)]
treeEdges (Node v chs) = map ((v,) . rootLabel) chs ++ concatMap treeEdges chs
