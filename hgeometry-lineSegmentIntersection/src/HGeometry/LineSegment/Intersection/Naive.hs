--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.LineSegment.Intersection.Naive
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Line segment intersections in \(O(n^2)\) by checking all pairs.
--
--------------------------------------------------------------------------------
module HGeometry.LineSegment.Intersection.Naive
  ( intersections
  , Intersections
  , Associated, startPointOf, endPointOf, interiorTo
  , associatedSegments

  , AroundEnd, AroundStart, AroundIntersection
  , isInteriorIntersection

  , IntersectionPoint
  , intersectionPointOf

  , intersectionPoint, associatedSegs

  , IntersectConstraints
  , OrdArounds
  ) where

import           Control.Lens ((^.))
import qualified Data.Map as Map
import           HGeometry.Combinatorial.Util
import           HGeometry.LineSegment
import           HGeometry.LineSegment.Intersection.Types
import           HGeometry.Point

--------------------------------------------------------------------------------

-- | Compute all intersections (naively)
--
-- \(O(n^2)\)
intersections :: ( Ord r, Fractional r
                 , LineSegment_ lineSegment point
                 , Eq lineSegment
                 , Point_ point 2 r
                 , IntersectConstraints seg lineSegment
                 , LineSegment_ seg point
                 , Foldable f
                 )
              => f lineSegment -> Intersections r lineSegment
intersections = foldMap collect . uniquePairs
  where
    collect (Two s s') = case intersectionPointOf s s' of
                           Nothing -> mempty
                           Just ip -> Map.singleton (ip^.intersectionPoint) (ip^.associatedSegs)

-- type R = Rational

-- seg1, seg2 :: LineSegment 2 () R
-- seg1 = ClosedLineSegment (ext $ Point2 0 0) (ext $ Point2 0 10)
-- seg2 = ClosedLineSegment (ext $ Point2 0 1) (ext $ Point2 0 5)
