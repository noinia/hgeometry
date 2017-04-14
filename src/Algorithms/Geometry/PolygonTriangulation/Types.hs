module Algorithms.Geometry.PolygonTriangulation.Types where

import           Control.Lens
import           Data.Ext
import qualified Data.Foldable as F
import           Data.Geometry.LineSegment
import           Data.Geometry.PlanarSubdivision
import           Data.PlanarGraph
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Semigroup

--------------------------------------------------------------------------------

data PolygonEdgeType = Original | Diagonal
                     deriving (Show,Read,Eq)


-- | Given a list of original edges and a list of diagonals, creates a
-- planar-subdivision
--
--
-- running time: \(O(n\log n)\)
constructSubdivision                :: (Num r, Ord r)
                                    => proxy s
                                    -> [LineSegment 2 p r] -- ^ original edges
                                    -> [LineSegment 2 p r] -- ^ diagonals
                                    -> PlanarSubdivision s p PolygonEdgeType () r
constructSubdivision px origs diags =
    subdiv&graph.vertexData.traverse.vData %~ NonEmpty.head
  where
    subdiv = fromConnectedSegments px $ origs' <> diags'
    diags' = (:+ EdgeData Visible Diagonal) <$> diags
    origs' = (:+ EdgeData Visible Original) <$> origs
