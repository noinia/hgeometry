-- We'll move this to Data.Geometry.Polygon.Monotone in the final version.
<<<<<<< HEAD
module Algorithms.Geometry.MonotonePolygon.MonotonePolygon(isMonotone, randomMonotone) where

import Control.Monad.Random
import Data.Geometry.Polygon.Core
import Data.Geometry.Vector
import Data.CircularSeq
import Data.List
module Algorithms.Geometry.MonotonePolygon.MonotonePolygon
  ( isMonotone
  , randomMonotone
  , randomMonotoneDirected
  ) where

import           Control.Monad.Random
import           Data.Ext
import qualified Data.Foldable              as F
import           Data.Geometry.Line         (Line (..))
import           Data.Geometry.LineSegment
import           Data.Geometry.Point
import           Data.Geometry.Polygon.Core
import           Data.Geometry.Vector
import           Data.Intersection

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
      match (edge `intersect` line) $
           H (\NoIntersection -> 0)
        :& H (\Point{} -> 1)
        -- This happens when an edge is parallel with the given direction.
        -- I think it's correct to count it as a single intersection.
        :& H (\LineSegment{} -> 1)
        :& RNil
      where
        line = Line pt direction

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
randomMonotone :: RandomGen g => Int -> Vector 2 Rational -> Rand g (SimplePolygon () Rational)
randomMonotone nVertices direction = polygon
    where
        -- 1, skip 2 in this function bc `direction` is given
        points = replicate createRandomPoint nVertices
        -- 3
        min = minimumBy (cmpExtreme direction) points
        max = maximumBy (cmpExtreme direction) points
        -- 4
        pointsWithoutExtremes = filter (\x -> x /= min && x /= max) points
        line = linearInterpolation min max
        positions = map line (map xCoord pointsWithoutExtremes)
        lineAndPoints = zip poistions pointsWithoutExtremes
        -- 5, 6
        leftHalf = sortBy (cmpExtreme direction) (filter (\(a,b) -> a <= b) lineAndPoints)
        rightHalf = sortBy (cmpExtreme (reverse direction)) (pointsWithoutExtremes \\ leftHalf)
        -- 7
        polygon = SimplePolygon $ Data.CircularSeq.fromList (min : leftHalf : max : rightHalf) 

-------------------------------------------------------------------------------------------------
-- helper functions

createRandomPoint :: Point 2 Rational
createRandomPoint = do
    let coords = createRandomRationalVec2
    let point = pointFromList coords :: Maybe (Point 2 Rational)
    case point of
        Just a -> a
        None -> origin :: Point 2 Rational
    return ()

createRandomRationalVec2 :: [Rational]
createRandomRationalVec2 = do
    g <- newStdGen
    map toRational (take 2 $ randoms g :: [Double])
    return ()

-- interpolate a line between p1 and p2 and yield the y value at a given x
linearInterpolation :: Point -> Point -> Rational -> Point
linearInterpolation p1 p2 x = 
    case point of
        Just a -> a
        None -> origin :: Point 2 Rational
    where
        slope = (yCoord p2) - (yCoord p1) / (xCoord p2) - (xCoord p1)
        offset = (yCoord p1) - (slope * (xcoord p1))
        point = pointFromList (x : [slope * x + offset]) :: Maybe (Point 2 Rational)

-- Pick a random vector and then call 'randomMonotone'.
-- | \( O(n \log n) \)
randomMonotoneDirected :: RandomGen g => Int -> Rand g (SimplePolygon () Rational)
randomMonotoneDirected nVertices = error "not implemented yet"
