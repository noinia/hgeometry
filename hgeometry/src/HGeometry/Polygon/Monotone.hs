--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Polygon.Monotone
-- Copyright   :  (C) 1ndy
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- A polygon is monotone in a certain direction if rays orthogonal to that
-- direction intersects the polygon at most twice. See
-- <https://en.wikipedia.org/wiki/Monotone_polygon>
--
--------------------------------------------------------------------------------
module HGeometry.Polygon.Monotone
  ( MonotonePolygon
  , MonotonePolygonF, monotoneDirection, toSimplePolygon
  , asMonotonePolygon
  , uncheckedMontonePolygon
  , randomMonotone
  , randomMonotoneDirected

  , isMonotone

  -- , monotoneFrom
  , randomNonZeroVector
  ) where

import           Control.Lens
import           Control.Monad (replicateM)
import           Control.Monad.State
import           Data.Foldable1
import           Data.Kind (Type)
import qualified Data.List as List
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           HGeometry.Intersection
import           HGeometry.Line.PointAndVector (LinePV(..))
import           HGeometry.LineSegment()
import           HGeometry.Point
import           HGeometry.Polygon.Class
import           HGeometry.Polygon.Simple
import           HGeometry.Properties
import           HGeometry.Vector
import           System.Random.Stateful

--------------------------------------------------------------------------------

-- | A type representing montone polygons
type MonotonePolygon       :: Type -> Type
type MonotonePolygon point = MonotonePolygonF (SimplePolygon point)

-- | Monotone polygons. A polygon is monotone in direction d if any
-- line perpendicular to direction d intersects the polygon in a
-- single interval.
--
data MonotonePolygonF simplePolygon =
  MonotonePolygon { monotoneDirection :: Vector 2 (NumType simplePolygon)
                  -- ^ the direction in which we are monotone
                  , toSimplePolygon  :: simplePolygon
                  -- ^ Convert to a simple polygon, i.e. forget the polygon is monotone
                  }
  -- deriving (NFData, Eq)

-- | Given a direction v, and a simple polygon that is monotone in
-- direction v, creates a Monotone polygon out of it. All of this is unchecked.
--
-- running time: O(1)
uncheckedMontonePolygon :: ( SimplePolygon_ simplePolygon point r
                           ) => Vector 2 r -> simplePolygon -> MonotonePolygonF simplePolygon
uncheckedMontonePolygon = MonotonePolygon


type instance Dimension (MonotonePolygonF simplePolygon) = 2
type instance NumType   (MonotonePolygonF simplePolygon) = NumType simplePolygon


-- | Try to establish that the given simple polygon is monotone with
-- respect to the given direction.
--
--
-- \( O(n \log n) \)
asMonotonePolygon  :: (SimplePolygon_ simplePolygon point r, Num r, Ord r
                      )
                   => Vector 2 r
                   -> simplePolygon
                   -> Maybe (MonotonePolygonF simplePolygon)
asMonotonePolygon v pg
  | isMonotone v pg = Just $ MonotonePolygon v pg
  | otherwise       = Nothing


-- | A polygon is monotone if a straight line in a given direction
--   cannot have more than two intersections.
--
-- \( O(n \log n) \)
isMonotone              :: ( SimplePolygon_ simplePolygon point r
                           , Num r, Ord r
                           )
                        => Vector 2 r -> simplePolygon -> Bool
isMonotone direction pg = all isMonotoneAt (pg^..vertices)
  -- Check for each vertex that the number of intersections with the
  -- line starting at the vertex and going out in the given direction
  -- intersects with the edges of the polygon no more than 2 times.
  where
    isMonotoneAt pt =
      sum (map (intersectionsThrough pt) (pg^..outerBoundaryEdgeSegments)) <= 2

    intersectionsThrough pt edge
      | line `intersects` edge = 1
      | otherwise              = 0
    -- note that if the line is the supporting line of the edge this counts as one intersection
      where
        line = LinePV (pt^.asPoint) (rot90 direction)
        rot90 (Vector2 x y) = Vector2 (-y) x

-- | Generate a random N-sided polygon that is monotone in a random direction.
--
-- -- pre: n >= 3
--
-- \( O(n \log n) \)
randomMonotone   :: (MonadState g m, RandomGen g, Uniform r, Ord r, Num r)
                 => Int -> m (MonotonePolygon (Point 2 r))
randomMonotone n = randomMonotoneDirected n =<< randomNonZeroVector
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

-- |
--   Generate a random n-sided polygon that is monotone in the given direction.
--
-- pre: n >= 3
--
-- \( O(n \log n) \)
randomMonotoneDirected             :: (MonadState g m, RandomGen g, Uniform r, Ord r, Num r)
                                   => Int -> Vector 2 r
                                   -> m (MonotonePolygon (Point 2 r))
randomMonotoneDirected n direction = do
    p      <- uniformM StateGenM
    points <- replicateM (n-1) (uniformM StateGenM)
    case monotoneFrom direction $ p :| points of
      Just pg -> pure pg
      Nothing -> error "randomMonotoneDirected: Absurd. fromPolygon failed"
  -- Pick a random vector and then call 'randomMonotone'.

-- | Assemble a given set of points in a polygon that is monotone in the given direction.
--
-- pre: at least 3 points are given
--
-- \( O(n \log n) \)
monotoneFrom               :: (Ord r, Num r, Foldable1 f, Point_ point 2 r, Eq point)
                           => Vector 2 r -> f point -> Maybe (MonotonePolygon point)
monotoneFrom direction pts = MonotonePolygon direction <$> thePolygon
  where
    thePolygon = fromPoints ([minP] <> rightHalf <> [maxP] <> leftHalf)

    minP = minimumBy (cmpInDirection2 direction) pts
    maxP = maximumBy (cmpInDirection2 direction) pts
    -- 4
    pointsWithoutExtremes = NonEmpty.filter (\p -> p /= minP && p /= maxP) $ toNonEmpty pts
    -- 5, 6
    (leftHalfUnsorted,rightHalfUnsorted) = List.partition (toTheLeft minP maxP) pointsWithoutExtremes
    leftHalf  = List.sortBy (flip $ cmpInDirection2 direction) leftHalfUnsorted
    rightHalf = List.sortBy (cmpInDirection2 direction)        rightHalfUnsorted

-------------------------------------------------------------------------------------------------
-- helper functions

-- for partitioning points
toTheLeft             :: (Point_ point 2 r, Ord r, Num r) => point -> point -> point -> Bool
toTheLeft minP maxP x = ccw minP maxP x == CCW

-- | \( O(1) \)
--   Create a random 2D vector which has a non-zero magnitude.
randomNonZeroVector :: (MonadState g m, RandomGen g, Uniform r, Eq r, Num r)
                    => m (Vector 2 r)
randomNonZeroVector = do
    v <- uniformM StateGenM
    if (quadrance v==0)
      then randomNonZeroVector
      else pure v
