module Algorithms.Geometry.EuclideanMST.EuclideanMST where

import           Algorithms.Geometry.DelaunayTriangulation.DivideAndConqueror
import           Algorithms.Geometry.DelaunayTriangulation.Types
import           Control.Lens
import           Data.Ext
import           Data.Geometry
import           Data.Geometry.Ipe
import qualified Data.List.NonEmpty as NonEmpty
import           Data.PlanarGraph
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
euclideanMST     :: (Ord r, Floating r)
                 => NonEmpty.NonEmpty (Point 2 r :+ p) -> Tree (Point 2 r :+ p)
euclideanMST pts = (\v -> g^.vDataOf v) <$> t
  where
    g = withEdgeDistances . toPlaneGraph (Proxy :: Proxy MSTW)
      . delaunayTriangulation $ pts
    t = mst g

-- extractEMST =


-- | Labels the edges of a plane graph with their distances
withEdgeDistances   :: Floating r
                    => PlaneGraph s w p e f r -> PlaneGraph s w p (r :+ e) f r
withEdgeDistances g = g&edgeData .~ xs
  where
    xs = fmap (\(d,x) -> len d :+ x) $ withEdgeData g
    len d = uncurry euclideanDist . over both (^.core) $ endPointData d g

data MSTW


drawTree' :: IpeOut (Tree (Point 2 r :+ p)) (IpeObject r)
drawTree' = IpeOut $
  asIpeGroup . map (asIpeObject' mempty . uncurry ClosedLineSegment) . treeEdges


treeEdges              :: Tree a -> [(a,a)]
treeEdges (Node v chs) = map ((v,) . rootLabel) chs ++ concatMap treeEdges chs
