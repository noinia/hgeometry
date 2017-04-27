module Algorithms.Geometry.LineSegmentIntersection where

import           Algorithms.Geometry.LineSegmentIntersection.Types
import qualified Algorithms.Geometry.LineSegmentIntersection.BentleyOttmann as BO
import           Data.Ext
import           Data.Geometry.LineSegment
import           Data.Geometry.Point
import           Data.Geometry.PolyLine
import           Data.Geometry.Polygon
import qualified Data.Map as Map

-- | Computes all interior properly intersections
interiorIntersections :: (Ord r, Fractional r)
                       => [LineSegment 2 p r] -> Intersections p r
interiorIntersections = Map.filter (not . isEndPointIntersection) . BO.intersections


-- | \(O(n \log n)\)
hasInteriorIntersections :: (Ord r, Fractional r)
                         => [LineSegment 2 p r] -> Bool
hasInteriorIntersections = not . null . interiorIntersections

-- isSelfIntersecting :: (Ord r, Fractional r) => PolyLine 2 p r -> Bool
-- isSelfIntersecting = hasInteriorIntersections . undefined


-- | \(O(n \log n)\)
hasSelfIntersections :: (Ord r, Fractional r) => Polygon t p r -> Bool
hasSelfIntersections = hasInteriorIntersections . listEdges
