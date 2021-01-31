--------------------------------------------------------------------------------
-- |
-- Module      :  Algorithms.Geometry.LineSegmentIntersection
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--------------------------------------------------------------------------------
module Algorithms.Geometry.LineSegmentIntersection
  ( hasInteriorIntersections
  , hasSelfIntersections
  , Intersections
  , Associated(..)
  , IntersectionPoint(..)
  , isEndPointIntersection
  , associated
  , Compare
  ) where

import qualified Algorithms.Geometry.LineSegmentIntersection.BentleyOttmann as BO
import           Algorithms.Geometry.LineSegmentIntersection.Types
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
