{-# LANGUAGE ScopedTypeVariables #-}
module Algorithms.Geometry.LineSegmentIntersection.Naive where

import           Algorithms.Geometry.LineSegmentIntersection.Types
import           Control.Lens
import           Data.Ext
import           Data.Geometry.Interval
import           Data.Geometry.LineSegment
import           Data.Geometry.Point
import           Data.Geometry.Properties
import qualified Data.Map as M
import           Data.Vinyl
import           Data.Vinyl.CoRec


-- | Compute all intersections (naively)
--
-- \(O(n^2)\)
intersections :: forall r p. (Ord r, Fractional r)
              => [LineSegment 2 p r] -> Intersections p r
intersections = foldr collect mempty . pairs

-- | Test if the two segments intersect, and if so add the segment to the map
collect          :: (Ord r, Fractional r)
                 => (LineSegment 2 p r, LineSegment 2 p r)
                 -> Intersections p r -> Intersections p r
collect (s,s') m = match (s `intersect` s') $
     (H $ \NoIntersection -> m)
  :& (H $ \p              -> handlePoint s s' p $ m)
  :& (H $ \s''            -> foldr (handlePoint s s') m [s''^.start.core, s''^.end.core])
  :& RNil

-- | Add s and s' to the map with key p
handlePoint        :: Ord r
                   => LineSegment 2 p r -> LineSegment 2 p r -> Point 2 r
                   -> Intersections p r -> Intersections p r
handlePoint s s' p = addTo p s . addTo p s'

-- | figure out which map to add the point to
addTo                  :: Ord r => Point 2 r -> LineSegment 2 p r
                       -> Intersections p r -> Intersections p r
addTo p s
  | p `isEndPointOf` s = M.insertWith (<>) p (associated [s] [])
  | otherwise          = M.insertWith (<>) p (associated [] [s])

isEndPointOf       :: Eq r => Point 2 r -> LineSegment 2 p r -> Bool
p `isEndPointOf` s = p == s^.start.core || p == s^.end.core


pairs        :: [a] -> [(a, a)]
pairs []     = []
pairs (x:xs) = map (x,) xs ++ pairs xs
