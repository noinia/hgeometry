--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Polygon.Visibility.Naive
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Naive algorithms to compute the Visibility graph and the Visibility polygon of a
-- particular point in a simple polgyon
--
--------------------------------------------------------------------------------
module HGeometry.Polygon.Visibility.Naive
  ( visibilityGraph
  , visibilityGraphWith
  , module HGeometry.Polygon.Visibility.NaiveVisibilityPolygon
  ) where

import Control.Lens
import Data.Coerce
import Data.Maybe (mapMaybe)
import HGeometry.Combinatorial.Util
import HGeometry.Ext
import HGeometry.Intersection
import HGeometry.LineSegment
import HGeometry.Point
import HGeometry.Polygon
import HGeometry.Vector
import HGeometry.Polygon.Visibility.NaiveVisibilityPolygon

--------------------------------------------------------------------------------

-- | Naive algorithm to compute the visibilityGraph of a simple polygon.
--
-- \(O(n^3)\)
visibilityGraph    :: ( SimplePolygon_ simplePolygon point r
                      , HasIntersectionWith point simplePolygon
                      , Ord r, Num r
                      )
                   => simplePolygon -> [Vector 2 (VertexIx simplePolygon)]
visibilityGraph pg = visibilityGraphWith pg
                   $ visibilityGraphWrtObstacles (pg^..outerBoundaryEdgeSegments)
                                                 (pg^..vertices.asIndexedExt)

-- | Given a polygon, and candidate visibility edges (edges guaranteed not to intersect)
-- the boundary; test if the edges are actually inside the polygon (as well as adding the
-- actual polygon edges as visible).
--
-- \(O(n + m)\), where m is the number of candidate edges.
visibilityGraphWith  :: ( SimplePolygon_ simplePolygon point r
                        , HasIntersectionWith point simplePolygon
                        , Ord r, Num r
                        )
                     => simplePolygon
                     -> [Vector 2 (point :+ VertexIx simplePolygon)] -- ^ candidate edges
                     -> [Vector 2 (VertexIx simplePolygon)]
visibilityGraphWith pg candidateEdges =
       (pg^..outerBoundaryEdges.asIndex.to (uncurry Vector2))
    <> mapMaybe liesInsidePolygon candidateEdges
  where
    liesInsidePolygon (Vector2 (u :+ i) (v :+ j)) = case cwCmpAroundWith (p .-. u) u v s of
                                                      LT -> Just (Vector2 i j)
                                                      _  -> Nothing
      where
        p = pg^.ccwPredecessorOf i
        s = pg^.ccwSuccessorOf i
        -- we check whether v is in between the angular interval defined by the neighbours
        -- of u.
  -- note: this uses the originetation of the simple polygon boundary
  -- for holes I guess we will have to use the ccw orientation

-- | computes the edges of the visibility graph among the points
--
-- O(n^2m), where n is the number of vertices, m is the number of obstacle edges.
visibilityGraphWrtObstacles           :: ( Foldable f, Foldable g
                                         , Point_ vertex 2 r
                                         , OpenLineSegment vertex
                                           `HasIntersectionWith` obstacleEdge
                                         )  => f obstacleEdge -> g vertex -> [Vector 2 vertex]
visibilityGraphWrtObstacles obstacles = coerce . filter areMutuallyVisible . uniquePairs
  where
    areMutuallyVisible (Two u v) =
      all (not . (intersects (OpenLineSegment u v))) obstacles
