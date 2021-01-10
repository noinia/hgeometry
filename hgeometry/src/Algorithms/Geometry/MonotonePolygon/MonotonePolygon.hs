-- We'll move this to Data.Geometry.Polygon.Monotone in the final version.
module Algorithms.Geometry.MonotonePolygon.MonotonePolygon where

import Control.Monad.Random
import Data.Geometry.Polygon.Core
import Data.Geometry.Vector

-- | \( O(n \log n) \)
--   A polygon is monotone if a straight line in a given direction
--   cannot have more than two intersections.
isMonotone :: Vector 2 r -> SimplePolygon p r -> Bool
isMonotone = error "not implemented yet"

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
randomMonotone nVertices direction = error "not implemented yet"

-- Pick a random vector and then call 'randomMonotone'.
-- | \( O(n \log n) \)
randomMonotoneAny :: RandomGen g => Int -> Rand g (SimplePolygon () Rational)
randomMonotoneAny nVertices = error "not implemented yet"
