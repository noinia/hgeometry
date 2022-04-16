{-# LANGUAGE ScopedTypeVariables #-}
-- | Line segment intersections in \(O(n^2)\) by checking
--   all pairs.
module Algorithms.Geometry.LineSegmentIntersection.Naive
  ( intersections
  ) where

import           Algorithms.Geometry.LineSegmentIntersection.Types
import           Control.Lens((^.))
import           Data.Ext
-- import           Data.Geometry.Interval
import           Data.Geometry.LineSegment
import           Data.Geometry.Point
import           Data.Geometry.Properties
import qualified Data.Map as M
import           Data.Util
import           Data.Vinyl
import           Data.Vinyl.CoRec
import qualified Data.List as List

--------------------------------------------------------------------------------

-- | Compute all intersections (naively)
--
-- \(O(n^2)\)
intersections :: forall r p e. (Ord r, Fractional r)
              => [LineSegment 2 p r :+ e] -> Intersections p r e
intersections = foldr collect mempty . uniquePairs

-- | Test if the two segments intersect, and if so add the segment to the map
collect              :: (Ord r, Fractional r)
                     => Two (LineSegment 2 p r :+ e)
                     -> Intersections p r e -> Intersections p r e
collect (Two s s') m = match ((s^.core) `intersect` (s'^.core)) $
     H (\NoIntersection -> m)
  :& H (\p              -> handlePoint s s' p m)
  :& H (\s''            -> handlePoint s s' (topEndPoint s'') m)
  :& RNil


topEndPoint :: Ord r => LineSegment 2 p r -> Point 2 r
topEndPoint (LineSegment' (a :+ _) (b :+ _)) = List.minimumBy ordPoints [a,b]


-- | Add s and s' to the map with key p
handlePoint        :: (Ord r, Fractional r)
                   => LineSegment 2 p r :+ e
                   -> LineSegment 2 p r :+ e
                   -> Point 2 r
                   -> Intersections p r e -> Intersections p r e
handlePoint s s' p = M.insertWith (<>) p (mkAssociated p s <> mkAssociated p s')


type R = Rational

seg1, seg2 :: LineSegment 2 () R
seg1 = ClosedLineSegment (ext $ Point2 0 0) (ext $ Point2 0 10)
seg2 = ClosedLineSegment (ext $ Point2 0 1) (ext $ Point2 0 5)
