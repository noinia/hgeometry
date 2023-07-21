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
  ) where

-- import           Algorithms.Geometry.LineSegmentIntersection.Types
import           Control.Lens ((^.))
import           HGeometry.Ext
import           HGeometry.LineSegment.Class
import           HGeometry.Point
import           HGeometry.Properties
import qualified Data.Map as Map
import           HGeometry.Combinatorial.Util
import qualified Data.List as List

--------------------------------------------------------------------------------

type Intersections lineSegment = [Point 2 (NumType lineSegment)]

-- | Compute all intersections (naively)
--
-- \(O(n^2)\)
intersections :: ( Ord r, Fractional r
                 , LineSegment_ lineSegment point
                 , Point_ point 2 r
                 )
              => [lineSegment] -> Intersections lineSegment
intersections = foldMap collect . uniquePairs

-- collect (Two s s') = case s `intersect`


-- -- | Test if the two segments intersect, and if so add the segment to the map
-- collect              :: (Ord r, Fractional r
--                         , LineSegment_ lineSegment 2 r
--                         )
--                      => Two lineSegment
--                      -> Intersections p r e
-- collect (Two s s') m = match ((s^.core) `intersect` (s'^.core)) $
--      H (\NoIntersection -> m)
--   :& H (\p              -> handlePoint s s' p m)
--   :& H (\s''            -> handlePoint s s' (topEndPoint s'') m)
--   :& RNil


-- topEndPoint :: Ord r => LineSegment 2 p r -> Point 2 r
-- topEndPoint (LineSegment' (a :+ _) (b :+ _)) = List.minimumBy ordPoints [a,b]


-- -- | Add s and s' to the map with key p
-- handlePoint        :: (Ord r, Fractional r)
--                    => LineSegment 2 p r :+ e
--                    -> LineSegment 2 p r :+ e
--                    -> Point 2 r
--                    -> Intersections p r e -> Intersections p r e
-- handlePoint s s' p = M.insertWith (<>) p (mkAssociated p s <> mkAssociated p s')


-- type R = Rational

-- seg1, seg2 :: LineSegment 2 () R
-- seg1 = ClosedLineSegment (ext $ Point2 0 0) (ext $ Point2 0 10)
-- seg2 = ClosedLineSegment (ext $ Point2 0 1) (ext $ Point2 0 5)
