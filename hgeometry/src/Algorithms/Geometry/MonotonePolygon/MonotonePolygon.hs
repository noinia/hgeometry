module Algorithms.Geometry.MonotonePolygon.MonotonePolygon
  ( isMonotone
  , randomMonotone
  , randomMonotoneDirected
  , monotoneFrom
  ) where

import           Control.Monad.Random
import           Data.Ext
import qualified Data.Foldable              as F
import           Data.Geometry.Line         (Line (..))
import           Data.Geometry.LineSegment
import           Data.Geometry.Point
import           Data.Geometry.Polygon.Core
import           Data.Geometry.Vector
import           Data.Geometry.Polygon.Extremes
import           Data.Geometry.Point.Orientation.Degenerate


import           Data.Intersection
import           Data.CircularSeq
import           Data.List
import           Data.Ratio

import Data.Vinyl
import Data.Vinyl.CoRec

-- | \( O(n \log n) \)
--   A polygon is monotone if a straight line in a given direction
--   cannot have more than two intersections.
isMonotone :: (Fractional r, Ord r) => Vector 2 r -> SimplePolygon p r -> Bool
-- Check for each vertex that the number of intersections with the
-- line starting at the vertex and going out in the given direction
-- intersects with the edges of the polygon no more than 2 times.
isMonotone direction p = all isMonotoneAt (map _core $ toPoints p)
  where
    isMonotoneAt pt =
      sum (map (intersectionsThrough pt) (F.toList $ outerBoundaryEdges p)) <= 2
    intersectionsThrough pt edge =
      match (Data.Intersection.intersect edge line) $
           H (\NoIntersection -> 0)
        :& H (\Point{} -> 1)
        -- This happens when an edge is parallel with the given direction.
        -- I think it's correct to count it as a single intersection.
        :& H (\LineSegment{} -> 1)
        :& RNil
      where
        line = Line pt (rot90 direction)
        rot90 (Vector2 x y) = Vector2 (-y) x

{- Algorithm overview:

  1. Create N `Point 2 Rational` (N >= 3)
  2. Create a random `Vector 2 Rational`
  3. Find the extremes (min and max) of the points when sorted in the direction of the vector.
      We already have code for this. See `maximumBy (cmpExtreme vector)` and
      `minimumBy (cmpExtreme vector)`.
  4. Take out the two extremal points from the set.
  5. Partition the remaining points according to whether they're on the left side or right side
    of the imaginary line between the two extremal points.
  6. Sort the two partitioned sets, one in the direction of the vector and one in the opposite
    direction.
  7. Connect the points, starting from the minimal extreme point, going through the set of points
    that are increasing in the direction of the vector, then to the maximal point, and finally
    down through the points that are decreasing in the direction of the vector.
-}
-- | \( O(n \log n) \)
randomMonotone :: (RandomGen g, Random r, Ord r, Num r) => Int -> Rand g (SimplePolygon () r)
randomMonotone nVertices = do
    points <- replicateM nVertices createRandomPoint
    direction <- generateRandomVector2
    return (monotoneFrom direction points)

-- Pick a random vector and then call 'randomMonotone'.
-- | \( O(n \log n) \)
randomMonotoneDirected :: (RandomGen g, Random r, Ord r, Num r)
  => Int -> Vector 2 r -> Rand g (SimplePolygon () r)
randomMonotoneDirected nVertices direction = do
    points <- replicateM nVertices createRandomPoint
    return (monotoneFrom direction points)        

-- General fuunction to create a monotone polygon
monotoneFrom :: (Ord r, Num r) => Vector 2 r -> [Point 2 r] -> SimplePolygon () r
monotoneFrom direction vertices = fromPoints ([min] ++ rightHalf ++ [max] ++ leftHalf)
    where
        specialPoints = map (\x -> x :+ ()) vertices
        min = Data.List.minimumBy (cmpExtreme direction) specialPoints
        max = Data.List.maximumBy (cmpExtreme direction) specialPoints
        -- 4
        pointsWithoutExtremes = filter (\x -> x /= min && x /= max) specialPoints
        -- 5, 6
        (leftHalfUnsorted,rightHalfUnsorted) = Data.List.partition (toTheLeft min max) pointsWithoutExtremes
        leftHalf = sortBy (cmpExtreme direction) leftHalfUnsorted
        rightHalf = reverse (sortBy (cmpExtreme direction) rightHalfUnsorted)

-------------------------------------------------------------------------------------------------
-- helper functions

-- for partitioning points
toTheLeft :: (Ord r, Num r) => Point 2 r :+ () -> Point 2 r :+ () -> Point 2 r :+ () -> Bool
toTheLeft min max x = Data.Geometry.Point.Orientation.Degenerate.ccw' min max x == CCW

-- create a single random point
createRandomPoint :: (RandomGen g, Random r) => Rand g (Point 2 r)
createRandomPoint = getRandom

-- create a random vector 2 for direction
generateRandomVector2 :: (RandomGen g, Random r, Eq r, Num r) => Rand g (Vector 2 r)
generateRandomVector2 = do
    v <- getRandom
    if (quadrance v==0)
      then generateRandomVector2
      else pure v
