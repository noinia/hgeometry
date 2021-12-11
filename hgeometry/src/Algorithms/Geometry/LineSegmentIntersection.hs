--------------------------------------------------------------------------------
-- |
-- Module      :  Algorithms.Geometry.LineSegmentIntersection
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--------------------------------------------------------------------------------
module Algorithms.Geometry.LineSegmentIntersection
  ( BooleanSweep.hasIntersections
  , BO.intersections
  , BO.interiorIntersections
  , Intersections
  , Associated(..)
  , associated
  , IntersectionPoint(..)
  , isEndPointIntersection
  , hasSelfIntersections
  ) where

import qualified Algorithms.Geometry.LineSegmentIntersection.BentleyOttmann as BO
import qualified Algorithms.Geometry.LineSegmentIntersection.BooleanSweep as BooleanSweep
import           Algorithms.Geometry.LineSegmentIntersection.Types
import           Data.Geometry.LineSegment
import           Data.Geometry.Polygon


-- | Test if the polygon has self intersections.
--
-- \(O(n \log n)\)
hasSelfIntersections :: (Ord r, Num r) => Polygon t p r -> Bool
hasSelfIntersections = BooleanSweep.hasIntersections . listEdges
