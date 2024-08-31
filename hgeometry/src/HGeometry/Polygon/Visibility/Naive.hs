--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Polygon.Visibility.Naive
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Naive algorithm to compute the Visibility graph in a simple polgyon
--
--------------------------------------------------------------------------------
module HGeometry.Polygon.Visibility.Naive
  ( visibilityGraph
  ) where

import Control.Lens
import Data.Maybe (mapMaybe)
import HGeometry.Combinatorial.Util
import HGeometry.Ext
import HGeometry.Intersection
import HGeometry.LineSegment
import HGeometry.Point
import HGeometry.Polygon
import HGeometry.Polygon.Simple
import HGeometry.Vector

--------------------------------------------------------------------------------

-- | Naive algorithm to compute the visibilityGraph of a simple polygon
--
-- O(n^3)
visibilityGraph     :: ( SimplePolygon_ simplePolygon point r
                       , HasIntersectionWith point simplePolygon
                       , Ord r, Fractional r
                        )
                    => simplePolygon -> [Vector 2 (VertexIx simplePolygon)]
visibilityGraph pg  = (uncurry Vector2 <$> pg^..outerBoundaryEdges.asIndex)
                      <> mapMaybe liesInsidePolygon candidateEdges
  where
    candidateEdges = visibilityGraph' (pg^..outerBoundaryEdgeSegments)
                                      ((\(i,v) -> v :+ i) <$> pg^..vertices.withIndex)
    liesInsidePolygon (Two (u :+ i) (v :+ j))
      | u .+^ ((v .-. u) ^/ 2) `intersects` pg = Just (Vector2 i j)
      | otherwise                              = Nothing


-- | computes the edges of the visibility graph among the points
--
-- O(n^2m), where n is the number of vertices, m is the number of obstacle edges.
visibilityGraph'           :: ( Foldable f, Foldable g
                              , Point_ vertex 2 r
                              , OpenLineSegment vertex `HasIntersectionWith` obstacleEdge
                              )  => f obstacleEdge -> g vertex -> [Two vertex]
visibilityGraph' obstacles = filter areMutuallyVisible . uniquePairs
  where
    areMutuallyVisible (Two u v) =
      all (not . (intersects (OpenLineSegment u v))) obstacles
