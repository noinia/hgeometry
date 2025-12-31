--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.ConvexHull.JarvisMarch
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--------------------------------------------------------------------------------
module HGeometry.ConvexHull.JarvisMarch(
    convexHull

  , upperHull, upperHull'
  , lowerHull, lowerHull'
  , steepestCcwFrom, steepestCwFrom
  ) where

import           Control.Lens ((^.), view)
import qualified Data.List as List
import           Data.List.NonEmpty (NonEmpty(..), (<|))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Ord (comparing, Down(..))
import           Data.Semigroup.Foldable
import           HGeometry.Ext
import           HGeometry.NonEmpty.Util (extractMinimaBy)
import           HGeometry.Point
import           HGeometry.Polygon.Convex
import           HGeometry.Polygon.Simple.Class
import           HGeometry.Vector

--------------------------------------------------------------------------------

-- | Compute the convexhull using JarvisMarch.
--
-- running time: \(O(nh)\), where \(n\) is the number of input points
-- and \(h\) is the complexity of the hull.
convexHull     :: (Ord r, Num r, Point_ point 2 r)
               => NonEmpty point -> ConvexPolygon point
convexHull pts = combine (upperHull pts) (lowerHull pts)
  where
    combine (NonEmpty.reverse -> _:|uh) (_:|lh) =
      uncheckedFromCCWPoints . NonEmpty.fromList  $ lh <> uh
  -- where
  --   SP p@(c :+ _) pts = minViewBy incXdecY ps
  --   takeWhile' pf (x :| xs) = x : takeWhile pf xs

-- | Computes the upper hull. The points are reported in left-to-right order.
--
-- running time: \(O(nh)\), where \(h\) is the complexity of the upper hull.
upperHull     ::  (Num r, Ord r, Point_ point 2 r) =>  NonEmpty point -> NonEmpty point
upperHull pts = repeatedly cmp steepestCwFrom s rest
  where
    (s:|_ :+ rest) = extractMinimaBy cmp pts
    cmp            = comparing (\(Point2_ x y) -> (x, Down y))
                    -- start from the topmost point that has minimum x-coord
                    -- also use cmp as the comparator, so that we also select the last
                    -- vertical segment.

-- | Upepr hull from left to right, without any vertical segments.
upperHull'     ::  (Num r, Ord r, Point_ point 2 r) =>  NonEmpty point -> NonEmpty point
upperHull' pts = pruneVertical $ repeatedly cmp steepestCwFrom s rest
  where
    (s:|_ :+ rest) = extractMinimaBy cmp0 pts
    cmp0           = comparing (\(Point2_ x y) -> (x, Down y))
                     -- start from the topmost point that has minimum x-coord
    cmp            = comparing (view asPoint)
                     -- for the rest select them in normal
                     -- lexicographic order, this causes the last
                     -- vertical segment to be ignored.

-- | Computes the lower hull, from left to right. Includes vertical
-- segments at the start.
--
-- running time: \(O(nh)\), where \(h\) is the complexity of the hull.
lowerHull     ::  (Num r, Ord r, Point_ point 2 r) =>  NonEmpty point -> NonEmpty point
lowerHull pts = pruneVertical $ repeatedly cmp steepestCcwFrom s rest
  where
    (s:|_ :+ rest) = extractMinimaBy cmp0 pts
    cmp0           = comparing (\(Point2_ x y) -> (x, Down y))
                     -- start from the topmost point that has minimum x-coord
    cmp            = comparing (view asPoint)
                     -- for the rest of the comparions use the normal
                     -- lexicographic comparing order.

-- | Jarvis March to compute the lower hull, without any vertical segments.
--
--
-- running time: \(O(nh)\), where \(h\) is the complexity of the hull.
lowerHull'     :: (Num r, Ord r, Point_ point 2 r) => NonEmpty point -> NonEmpty point
lowerHull' pts = pruneVertical $ repeatedly cmp steepestCcwFrom s rest
  where
    (s:|_ :+ rest) = extractMinimaBy cmp pts
    cmp            = comparing (view asPoint)


-- | Find the next point in counter clockwise order, i.e. the point
-- with minimum slope w.r.t. the given point. For points on the same distance we report
-- the furthest point.
steepestCcwFrom   :: (Ord r, Num r, Point_ point 2 r)
                  => point  -> NonEmpty point  -> point
steepestCcwFrom p = List.minimumBy (ccwCmpAroundWith (Vector2 0 (-1)) p
                                    <> flip (cmpByDistanceTo p)
                                   )


-- | Find the next point in clockwise order, i.e. the point
-- with maximum slope w.r.t. the given point.
steepestCwFrom   :: (Ord r, Num r, Point_ point 2 r)
                 => point -> NonEmpty point -> point
steepestCwFrom p = List.minimumBy (cwCmpAroundWith (Vector2 0 1) p
                                    <> flip (cmpByDistanceTo p)
                                  )

repeatedly       :: (a -> a -> Ordering) -> (a -> NonEmpty a -> a) -> a -> [a] -> NonEmpty a
repeatedly cmp f = go
  where
    go m xs' = case NonEmpty.nonEmpty xs' of
      Nothing -> m :| []
      Just xs -> let p = f m xs
                 in m <| go p (NonEmpty.filter (\x -> p `cmp` x == LT) xs)


-- | Removes the topmost vertical points, if they exist
pruneVertical :: (Eq r, Point_ point 2 r) => NonEmpty point -> NonEmpty point
pruneVertical = either id id . foldr1With f (\q -> Left $ q:|[])
  where
    f p = \case
      Left (q:|qs) | p^.xCoord == q^.xCoord -> Left  (p :| qs)
                   | otherwise              -> Right (p :| q:qs)
      Right pts                             -> Right (p <| pts)

-- | Foldr, but start by applying some function on the rightmost
-- element to get the starting value.
foldr1With     :: Foldable1 f => (a -> b -> b) -> (a -> b) -> f a -> b
foldr1With f b = go . toNonEmpty
  where
    go (x :| xs) = case NonEmpty.nonEmpty xs of
                     Nothing  -> b x
                     Just xs' -> x `f` go xs'
