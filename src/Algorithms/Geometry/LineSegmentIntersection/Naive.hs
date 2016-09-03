{-# LANGUAGE ScopedTypeVariables #-}
module Algorithms.Geometry.LineSegmentIntersection.BentleyOttmann where

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


intersections :: forall r p. (Ord r, Fractional r)
              => [LineSegment 2 p r] -> [IntersectionPoint p r]
intersections = output . foldr collect (mempty,mempty) . pairs

type PMap p r = M.Map (Point 2 r) [LineSegment 2 p r]
type Maps p r = (PMap p r, PMap p r)

output                         :: Ord r => Maps p r -> [IntersectionPoint p r]
output (endPtMap, interiorMap) =
    map (\p -> IntersectionPoint p (lookup' p endPtMap) (lookup' p interiorMap)) pts
  where
    pts = S.toList $ M.keysSet endPtMap `S.union` M.keysSet interiorMap
    lookup' k m = fromMaybe [] $ M.lookup k m

isEndPointOf :: Eq r => Point 2 r -> LineSegment 2 p r -> Bool
p `isEndPointOf` s = p == s^.start.core || p == s^.end.core

addTo'     :: Ord k => k -> v -> M.Map k [v] -> M.Map k [v]
addTo' p s = M.insertWith (++) p [s]

addTo                  :: Ord r => Point 2 r -> LineSegment 2 p r
                       -> Maps p r -> Maps p r
addTo p s (endPtMap, interiorMap)
  | p `isEndPointOf` s = (addTo' p s endPtMap, interiorMap)
  | otherwise          = (endPtMap,            addTo' p s interiorMap)

handlePoint        :: Ord r
                   => LineSegment 2 p r -> LineSegment 2 p r -> Point 2 r
                   -> Maps p r -> Maps p r
handlePoint s s' p = addTo p s . addTo p s'

collect          :: (Ord r, Fractional r)
                 => (LineSegment 2 p r, LineSegment 2 p r) -> Maps p r -> Maps p r
collect (s,s') m = match (s `intersect` s') $
     (H $ \NoIntersection -> m)
  :& (H $ \p              -> handlePoint s s' p $ m)
  :& (H $ \s''            -> foldr (handlePoint s s') m [s''^.start.core, s''^.end.core])
  :& RNil


pairs        :: [a] -> [(a, a)]
pairs []     = []
pairs (x:xs) = map (x,) xs ++ pairs xs
