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
  , Associated(..), startPointOf, endPointOf, interiorTo
  , IntersectionPoint(..), mkIntersectionPoint
  , intersectionPoint, associatedSegs
  -- , isInteriorIntersection
  , hasSelfIntersections

  , AroundStart(..), AroundEnd(..), AroundIntersection(..)
  ) where

import qualified Algorithms.Geometry.LineSegmentIntersection.BentleyOttmann as BO
import qualified Algorithms.Geometry.LineSegmentIntersection.BooleanSweep as BooleanSweep
import           Algorithms.Geometry.LineSegmentIntersection.Types
import           Data.Ext (ext)
import           Geometry.LineSegment
import           Geometry.Polygon

import qualified Data.Map as Map

-- | Test if the polygon has self intersections.
--
-- \(O(n \log n)\)
hasSelfIntersections :: (Ord r, Fractional r) => Polygon t p r -> Bool
hasSelfIntersections = not . Map.null . BO.interiorIntersections . map ext . listEdges
-- hasSelfIntersections :: (Ord r, Num r) => Polygon t p r -> Bool
-- hasSelfIntersections = BooleanSweep.hasIntersections . listEdges
-- FIXME: fix the open/closed bug, then switch to a boolean sweep based version
