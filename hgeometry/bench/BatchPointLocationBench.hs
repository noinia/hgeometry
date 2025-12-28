module Main( main ) where

import Control.DeepSeq
import Data.Word
import HGeometry.Number.Real.Rational
import HGeometry.Kernel
import Control.Lens
import System.Random
import Data.Foldable1
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty(..))
import Data.List.NonEmpty qualified as NonEmpty
import Test.Tasty.Bench
import HGeometry.Plane.BatchPointLocation
import Data.Map.NonEmpty qualified as NEMap
import Data.Set qualified as Set

--------------------------------------------------------------------------------

type R = RealNumber 5

--------------------------------------------------------------------------------

-- | pre: n > 0
--
-- renerates n random points, taken uniformly at random.
randomPoints   :: ( Uniform point
                  ) => Int -> IO (NonEmpty point)
randomPoints n = NonEmpty.fromList . take n . points <$> getStdGen
  where
    points = List.unfoldr (Just . uniform)

-- | Generates random planes
randomPlanes   :: Int -> IO (NonEmpty (Plane R))
randomPlanes n = NonEmpty.fromList . take n . planes <$> getStdGen
  where
    planes = List.unfoldr (Just . genPlane)

    genPlane     :: RandomGen gen => gen -> (Plane R, gen)
    genPlane gen = let (pts, gen') = uniform gen
                   in case fromPoints (pts&mapped %~ fromPoint) of
                        Nothing -> genPlane gen'
                        Just h  -> (h,gen')

fromPoints :: forall r. (Ord r, Fractional r) => Vector 3 (Point 3 r) -> Maybe (Plane r)
fromPoints = asNonVerticalHyperPlane @(HyperPlane 3 r) . hyperPlaneThrough


--------------------------------------------------------------------------------

-- | Helper function to generate uniformly random points at integer
-- coordinates in an [0,127]^3 grid.
fromPoint   :: Point 3 Word8 -> Point 3 R
fromPoint p = p&coordinates %~ fromIntegral


--------------------------------------------------------------------------------


naivePlanesAbove                :: forall queryPoint r plane set.
                                   ( Point_ queryPoint 3 r
                                   , Plane_ plane r
                                   , Foldable set
                                   , Ord r, Fractional r
                                   , Ord plane
                                   , Ord queryPoint
                                   )
                                => NonEmpty queryPoint
                                -> set plane
                                -> NEMap.NEMap queryPoint (Set.Set plane)
naivePlanesAbove queries planes =
    foldMap1 (\q -> NEMap.singleton q (Set.filter (q `liesBelow`) planes')) queries
  where
    planes' = foldMap Set.singleton planes

    liesBelow       :: queryPoint -> plane -> Bool
    q `liesBelow` h = verticalSideTest q h /= GT

--------------------------------------------------------------------------------


main :: IO ()
main = do
  let r = 10
      n = 1000
  planes  <- force                      <$> randomPlanes r
  queries <- force . fmap fromPoint     <$> randomPoints n
  -- let vd = voronoiDiagram pts
  -- print vd
  defaultMain
    [ bgroup "Batched point location/computing conflict lists Benchmarks"
        [ bench "Via Batched PointLoc" $ nf (uncurry batchedPointLocation) (queries, planes)
        , bench "Brute Force"          $ nf (uncurry naivePlanesAbove)     (queries, planes)
        ]
    ]
