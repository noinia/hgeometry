{-# LANGUAGE ScopedTypeVariables #-}
-- | Line segment intersections in \(O(n^2)\) by checking
--   all pairs.
module Algorithms.Geometry.LineSegmentIntersection.Naive
  ( intersections
  ) where

import           Algorithms.Geometry.LineSegmentIntersection.Types
import           Control.Lens
import           Data.Ext
import           Data.Geometry.Interval
import           Data.Geometry.LineSegment
import           Data.Geometry.Point
import           Data.Geometry.Properties
import qualified Data.Map as M
import           Data.Util
import           Data.Vinyl
import           Data.Vinyl.CoRec

-- | Compute all intersections (naively)
--
-- \(O(n^2)\)
intersections :: forall r p. (Ord r, Fractional r)
              => [LineSegment 2 p r] -> Intersections p r
intersections = foldr collect mempty . uniquePairs

-- | Test if the two segments intersect, and if so add the segment to the map
collect              :: (Ord r, Fractional r)
                     => Two (LineSegment 2 p r)
                     -> Intersections p r -> Intersections p r
collect (Two s s') m = match (s `intersect` s') $
     H (\NoIntersection -> m)
  :& H (\p              -> handlePoint s s' p m)
  :& H (\s''            -> foldr (handlePoint s s') m [s''^.start.core, s''^.end.core])
  :& RNil

-- | Add s and s' to the map with key p
handlePoint        :: (Ord r, Fractional r)
                   => LineSegment 2 p r
                   -> LineSegment 2 p r
                   -> Point 2 r
                   -> Intersections p r -> Intersections p r
handlePoint s s' p = M.insertWith (<>) p (mkAssociated p s <> mkAssociated p s')
