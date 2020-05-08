module Algorithms.Geometry.ConvexHull.JarvisMarch(
    convexHull

  , upperHull, upperHull'
  , lowerHull, lowerHull'
  , steepestCcwFrom, steepestCwFrom
  ) where

import           Control.Lens ((^.))
import           Data.Bifunctor
import           Data.Either (either)
import           Data.Ext
import           Data.Foldable
import           Data.Geometry.Point
import           Data.Geometry.Polygon
import           Data.Geometry.Polygon.Convex (ConvexPolygon(..))
import           Data.Geometry.Vector
import qualified Data.List as List
import           Data.List.NonEmpty (NonEmpty(..), (<|))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Ord (comparing, Down(..))
import           Data.Semigroup.Foldable

--------------------------------------------------------------------------------

-- | Compute the convexhull using JarvisMarch. The resulting polygon
-- is given in clockwise order.
--
-- running time: \(O(nh)\), where \(n\) is the number of input points
-- and \(h\) is the complexity of the hull.
convexHull            :: (Ord r, Num r)
                      => NonEmpty (Point 2 r :+ p) -> ConvexPolygon p r
convexHull (p :| []) = ConvexPolygon . fromPoints $ [p]
convexHull pts       = ConvexPolygon . fromPoints $ uh <> reverse lh
  where
    lh = case NonEmpty.nonEmpty (NonEmpty.init $ lowerHull pts) of
           Nothing       -> []
           Just (_:|lh') -> lh'
    uh = toList $ upperHull pts

                       -- note that fromList is afe since ps contains at least two elements
  -- where
  --   SP p@(c :+ _) pts = minViewBy incXdecY ps
  --   takeWhile' pf (x :| xs) = x : takeWhile pf xs

upperHull     ::  (Num r, Ord r) =>  NonEmpty (Point 2 r :+ p) -> NonEmpty (Point 2 r :+ p)
upperHull pts = repeatedly cmp steepestCwFrom s rest
  where
    (s:_ :+ rest) = extractMinimaBy cmp (NonEmpty.toList pts)
    cmp           = comparing (\(Point2 x y :+ _) -> (x, Down y))
                    -- start from the topmost point that has minimum x-coord
                    -- also use cmp as the comparator, so that we also select the last
                    -- vertical segment.

-- | Upepr hull from left to right, without any vertical segments.
upperHull'     ::  (Num r, Ord r) =>  NonEmpty (Point 2 r :+ p) -> NonEmpty (Point 2 r :+ p)
upperHull' pts = pruneVertical $ repeatedly cmp steepestCwFrom s rest
  where
    (s:_ :+ rest) = extractMinimaBy cmp0 (NonEmpty.toList pts)
    cmp0          = comparing (\(Point2 x y :+ _) -> (x, Down y))
                    -- start from the topmost point that has minimum x-coord
    cmp           = comparing (^.core)
                    -- for the rest select them in normal
                    -- lexicographic order, this causes the last
                    -- vertical segment to be ignored.

-- | Computes the lower hull, from left to right. Includes vertical
-- segments at the start.
--
-- running time: \(O(nh)\), where \(h\) is the complexity of the hull.
lowerHull     ::  (Num r, Ord r) =>  NonEmpty (Point 2 r :+ p) -> NonEmpty (Point 2 r :+ p)
lowerHull pts = pruneVertical $ repeatedly cmp steepestCcwFrom s rest
  where
    (s:_ :+ rest) = extractMinimaBy cmp0 (NonEmpty.toList pts)
    cmp0          = comparing (\(Point2 x y :+ _) -> (x, Down y))
                    -- start from the topmost point that has minimum x-coord
    cmp           = comparing (^.core)
                    -- for the rest of the comparions use the normal
                    -- lexicographic comparing order.

-- | Jarvis March to compute the lower hull, without any vertical segments.
--
--
-- running time: \(O(nh)\), where \(h\) is the complexity of the hull.
lowerHull'     :: (Num r, Ord r) => NonEmpty (Point 2 r :+ p) -> NonEmpty (Point 2 r :+ p)
lowerHull' pts = pruneVertical $ repeatedly cmp steepestCcwFrom s rest
  where
    (s:_ :+ rest) = extractMinimaBy cmp (NonEmpty.toList pts)
    cmp           = comparing (^.core)


-- | Find the next point in counter clockwise order, i.e. the point
-- with minimum slope w.r.t. the given point.
steepestCcwFrom   :: (Ord r, Num r)
               => (Point 2 r :+ a) -> NonEmpty (Point 2 r :+ b)  -> Point 2 r :+ b
steepestCcwFrom p = List.minimumBy (ccwCmpAroundWith (Vector2 0 (-1)) p)

-- | Find the next point in clockwise order, i.e. the point
-- with maximum slope w.r.t. the given point.
steepestCwFrom   :: (Ord r, Num r)
               => (Point 2 r :+ a) -> NonEmpty (Point 2 r :+ b)  -> Point 2 r :+ b
steepestCwFrom p = List.minimumBy (cwCmpAroundWith (Vector2 0 1) p)

repeatedly       :: (a -> a -> Ordering) -> (a -> NonEmpty a -> a) -> a -> [a] -> NonEmpty a
repeatedly cmp f = go
  where
    go m xs' = case NonEmpty.nonEmpty xs' of
      Nothing -> m :| []
      Just xs -> let p = f m xs
                 in m <| go p (NonEmpty.filter (\x -> p `cmp` x == LT) xs)


-- | Removes the topmost vertical points, if they exist
pruneVertical :: Eq r => NonEmpty (Point 2 r :+ p) -> NonEmpty (Point 2 r :+ p)
pruneVertical = either id id . foldr1With f (\q -> Left $ q:|[])
  where
    f p = \case
      Left (q:|qs) | p^.core.xCoord == q^.core.xCoord -> Left  (p :| qs)
                   | otherwise                        -> Right (p :| q:qs)
      Right pts                                       -> Right (p <| pts)

-- | Foldr, but start by applying some function on the rightmost
-- element to get the starting value.
foldr1With     :: Foldable1 f => (a -> b -> b) -> (a -> b) -> f a -> b
foldr1With f b = go . toNonEmpty
  where
    go (x :| xs) = case NonEmpty.nonEmpty xs of
                     Nothing  -> b x
                     Just xs' -> x `f` (go xs')

-- | extracts all minima from the list. The result consists of the
-- list of minima, and all remaining points. Both lists are returned
-- in the order in which they occur in the input.
--
-- >>> extractMinimaBy compare [1,2,3,0,1,2,3,0,1,2,0,2]
-- [0,0,0] :+ [2,3,1,2,3,1,2,1,2]
extractMinimaBy     :: (a -> a -> Ordering) -> [a] -> [a] :+ [a]
extractMinimaBy cmp = \case
  []     -> [] :+ []
  (x:xs) -> first NonEmpty.toList $ foldr (\y (mins@(m:|_) :+ rest) ->
                                             case m `cmp` y of
                                               LT -> mins :+ y:rest
                                               EQ -> (y NonEmpty.<| mins) :+ rest
                                               GT -> (y:|[]) :+ NonEmpty.toList mins <> rest
                                          ) ((x:|[]) :+ []) xs
