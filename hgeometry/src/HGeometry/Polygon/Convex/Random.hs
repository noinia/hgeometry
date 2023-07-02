module HGeometry.Polygon.Convex.Random
  ( randomConvex
  ) where

import           Control.DeepSeq (NFData)
import           Control.Lens
import           Control.Monad.ST
import           Control.Monad.State
import           Data.Coerce
import qualified Data.Foldable as F
import           Data.Function (on)
import qualified Data.IntSet as IS
import           Data.List.NonEmpty (NonEmpty (..))
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Maybe (fromJust)
import           Data.Ord (comparing)
import           Data.Semigroup.Foldable (Foldable1 (..))
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as Mut
import qualified Data.Vector.NonEmpty as NE
import qualified Data.Vector.Unboxed as VU
import           HGeometry.Ext
import           HGeometry.Number.Radical
import           HGeometry.Point
import           HGeometry.Point.Class
import           HGeometry.Polygon.Class
import           HGeometry.Polygon.Convex.Class
import           HGeometry.Polygon.Convex.Implementation
import           HGeometry.Polygon.Simple.Class
import           HGeometry.Vector
import           Hiraffe.Graph
import           System.Random.Stateful

--------------------------------------------------------------------------------
-- Random convex polygons

-- This is true for all convex polygons:
--   1. the sum of all edge vectors is (0,0). This is even true for all polygons.
--   2. edges are sorted by angle. Ie. all vertices are convex, not reflex.
--
-- So, if we can generate a set of vectors that sum to zero then we can sort them
-- and place them end-to-end and the result will be a convex polygon.
--
-- So, we need to generate N points that sum to 0. This can be done by generating
-- two sets of N points that sum to M, and the subtracting them from each other.
--
-- Generating N points that sum to M is done like this: Generate (N-1) unique points
-- between (but not including) 0 and M. Write down the distance between the points.
-- Imagine a scale from 0 to M:
--   0            M
--   |            |
-- Then we add two randomly selected points:
--   0            M
--   |  *      *  |
-- Then we look at the distance between 0 and point1, point1 and point2, and point2 to M:
--   0            M
--   |--*------*--|
--    2     6    2
-- 2+6+2 = 10 = M
--
-- Doing this again might yield [5,2,3]. Subtract them:
--     [2,   6,   2  ]
--   - [5,   2,   3  ]
--   = [2-5, 6-2, 2-3]
--   = [-3,  4,   -1 ]
-- And the sum of [-3, 4, -1] = -3+4-1 = 0.

-- O(n log n)
randomBetween                     :: (MonadState g m, RandomGen g)
                                  => Int -> Int -> m (VU.Vector Int)
randomBetween n vMax | vMax < n+1 = pure $ VU.replicate vMax 1
randomBetween n vMax              = worker (n-1) IS.empty
  where
    gen from []     = [vMax-from]
    gen from (x:xs) = (x-from) : gen x xs
    worker 0 seen = pure (VU.fromList (gen 0 $ IS.elems seen))
    worker i seen = do
      v <- uniformRM (1, vMax-1) StateGenM
      if IS.member v seen
        then worker i seen
        else worker (i-1) (IS.insert v seen)

randomBetweenZero        :: (MonadState g m, RandomGen g) => Int -> Int -> m (VU.Vector Int)
randomBetweenZero n vMax = VU.zipWith (-) <$> randomBetween n vMax <*> randomBetween n vMax

randomEdges        :: (MonadState g m, RandomGen g) => Int -> Int -> m [Point 2 Int]
randomEdges n vMax = do
  zipWith Point2
    <$> fmap VU.toList (randomBetweenZero n vMax)
    <*> fmap VU.toList (randomBetweenZero n vMax)

-- | Generate a random ConvexPolygon with @N@ vertices and a granularity of @vMax@.
--
-- \( O(n \log n) \)
randomConvex                 :: (MonadState g m, RandomGen g)
                             => Int -> Int -> m (ConvexPolygon (Point 2 Rational))
randomConvex n _vMax | n < 3 =
  error "HGeometry.Polygon.Convex.randomConvex: At least 3 edges are required."
randomConvex n vMax         = do
  ~(v:vs) <- sortAround origin <$> randomEdges n vMax
  let theVertices = over coordinates ((/ realToFrac vMax) . realToFrac)
                 <$> NonEmpty.scanl (\p u -> p .+^ (u^.vector)) v vs
      pRational = uncheckedFromCCWPoints theVertices
      Point c   = centroid pRational
  pure $ pRational&outerBoundary %~ (.-^ c)
