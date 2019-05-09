module Algorithms.Geometry.LineSegmentIntersection where

import qualified Algorithms.Geometry.LineSegmentIntersection.BentleyOttmann as BO
import           Data.Geometry.LineSegment
import           Data.Geometry.Polygon

-- Tests if there are any interior intersections.
--
-- | \(O(n \log n)\)
hasInteriorIntersections :: (Ord r, Fractional r)
                         => [LineSegment 2 p r] -> Bool
hasInteriorIntersections = not . null . BO.interiorIntersections

-- | \(O(n \log n)\)
hasSelfIntersections :: (Ord r, Fractional r) => Polygon t p r -> Bool
hasSelfIntersections = hasInteriorIntersections . listEdges
