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
import           Data.Maybe (fromMaybe)
import qualified Data.Set as S
import           Data.Vinyl
import           Frames.CoRec

import Debug.Trace

-- | Compute all intersections (naively)
--
-- $O(n^2)$
intersections :: forall r p. (Ord r, Fractional r, Show r, Show p)
              => [LineSegment 2 p r] -> [IntersectionPoint p r]
intersections = output . traceShowId . foldr collect (mempty,mempty) . pairs



type PMap p r = M.Map (Point 2 r) [LineSegment 2 p r]
type Maps p r = (PMap p r, PMap p r)

-- | Convert the maps into a list of intersection points
output                         :: Ord r => Maps p r -> [IntersectionPoint p r]
output (endPtMap, interiorMap) =
    map (\p -> IntersectionPoint p (lookup' p endPtMap) (lookup' p interiorMap)) pts
  where
    pts = S.toList $ M.keysSet endPtMap `S.union` M.keysSet interiorMap
    lookup' k m = fromMaybe [] $ M.lookup k m

-- | Test if the two segments intersect. If it is an endpoint add it to that
-- map, otherwise (an interior point) add it to the map of interior points.
collect          :: (Ord r, Fractional r)
                 => (LineSegment 2 p r, LineSegment 2 p r) -> Maps p r -> Maps p r
collect (s,s') m = match (s `intersect` s') $
     (H $ \NoIntersection -> m)
  :& (H $ \p              -> handlePoint s s' p $ m)
  :& (H $ \s''            -> foldr (handlePoint s s') m [s''^.start.core, s''^.end.core])
  :& RNil


-- | Add s and s' to the map with key p
handlePoint        :: Ord r
                   => LineSegment 2 p r -> LineSegment 2 p r -> Point 2 r
                   -> Maps p r -> Maps p r
handlePoint s s' p = addTo p s . addTo p s'


-- | figure out which map to add the point to
addTo                  :: Ord r => Point 2 r -> LineSegment 2 p r
                       -> Maps p r -> Maps p r
addTo p s (endPtMap, interiorMap)
  | p `isEndPointOf` s = (addTo' p s endPtMap, interiorMap)
  | otherwise          = (endPtMap,            addTo' p s interiorMap)


isEndPointOf       :: Eq r => Point 2 r -> LineSegment 2 p r -> Bool
p `isEndPointOf` s = p == s^.start.core || p == s^.end.core

--------------------------------------------------------------------------------

-- | Insert with append
addTo'     :: Ord k => k -> v -> M.Map k [v] -> M.Map k [v]
addTo' p s = M.insertWith (++) p [s]

pairs        :: [a] -> [(a, a)]
pairs []     = []
pairs (x:xs) = map (x,) xs ++ pairs xs
