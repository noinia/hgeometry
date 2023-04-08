module ConvexHull.Bench (
    runBenchmark
  , benchmark
  -- , runProfile
  ) where

import qualified HGeometry.ConvexHull.DivideAndConquer as DivideAndConquer
import qualified HGeometry.ConvexHull.GrahamScan as GrahamScan
import qualified HGeometry.ConvexHull.JarvisMarch as JarvisMarch
import qualified HGeometry.ConvexHull.QuickHull as QuickHull
import qualified ConvexHull.GrahamV2 as GrahamV2
import qualified ConvexHull.GrahamInt as GrahamInt
import qualified ConvexHull.GrahamFastest as GrahamFastest
  -- hand written implementation for Int, this should be the fastest possible somehow.
  -- FIXME: currently still uses merge-sort, switch to quicksort/introsort

import           Control.DeepSeq
-- import qualified ConvexHull.GrahamClassy as GrahamClassy
import           HGeometry.Foldable.Sort
import qualified Data.Vector.Unboxed as UV
import           System.Random
-- import           Data.Double.Approximate
import           HGeometry.Ext
import           Control.Lens
import           HGeometry.Point
import           HGeometry.Polygon.Class
import           HGeometry.Polygon.Convex
import           HGeometry.Vector
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import           HGeometry.Number.Real.Rational
import           Test.Tasty.Bench
import           Data.Hashable
import qualified Data.List as List
import           System.Random.Stateful (Uniform(..), UniformRange(..))
import           Util

--------------------------------------------------------------------------------

type R = RealNumber 5


gen :: StdGen
gen = genByName "convex hull"

--------------------------------------------------------------------------------

runBenchmark :: IO ()
runBenchmark = do
    let intPts = randomPoints @(Point 2 Int) gen 100000
    defaultMain [ benchmark intPts
                ]

-- asPoints   :: [Point 2 Int] -> [point]
-- asPoints n = NonEmpty.fromList . take n

-- | Benchmark building the convexHull
benchmark    :: NonEmpty (Point 2 Int) -> Benchmark
benchmark intPts = bgroup "ConvexHull" $ []
  --     [ bgroup ("1e"++show i ++ "/RealNumber") (convexHullFractional $ evalRand (genPts @R n) gen)
  --     | i <- [3, 4::Int]
  --     , let n = 10^i
  --     ] ++
  --     [ bgroup ("1e"++show i ++ "/Int") (convexHullNum $ evalRand (genPts @Int n) gen)
  --     | i <- [4, 5::Int]
  --     , let n = 10^i
  --     ] ++
  --     [ bgroup ("1e"++show i ++ "/Double") (convexHullFractional $ evalRand (genPts @Double n) gen)
  --     | i <- [4, 5::Int]
  --     , let n = 10^i ]
  -- where
  --   convexHullFractional pts =
  --               [ bench "GrahamScan" $ nf GrahamScan.convexHull pts
  --               , bench "DivideAndConquer" $ nf DivideAndConquer.convexHull pts
  --               , bench "QuickHull" $ nf QuickHull.convexHull pts
  --               , bench "JarvisMarch" $ nf JarvisMarch.convexHull pts
  --               ]
  --   convexHullNum pts =
  --               [ bench "GrahamScan" $ nf GrahamScan.convexHull pts
  --               , bench "DivideAndConquer" $ nf DivideAndConquer.convexHull pts
  --               , bench "JarvisMarch" $ nf JarvisMarch.convexHull pts
  --               ]



-- -- | Benchmark building the convexHull
-- benchmark                  :: NFData point
--                            => String -- ^ name
--                            -> [Int] -- ^ sizes i s.t. size is actually 10^i
--                            -- -> (NonEmpty point -> [Benchmark])  -- ^ function to benchmark
--                            -> NonEmpty point -- ^ sufficiently log list of points
--                            -> Benchmark
-- benchmark name sizes f pts = bgroup "ConvexHull" $
--     -- [ bgroup ("1e" <> show i <> name) (f . force $ take' n pts)
--     -- | i <- sizes
--     -- , let n = 10^i
--     -- ] <>
--     [ bgroup ("1e"++show i ++ "/RealNumber") (convexHullFractional $ genPts @R n)
--     | i <- [3, 4::Int]
--     , let n = 10^i
--     ] ++
--     [ bgroup ("1e"++show i ++ "/Int:+()") (convexHullNum $ fmap ext $ genPts @Int n)
--     | i <- [4, 5::Int]
--     , let n = 10^i
--     ] ++
--     [ bgroup ("1e"++show i ++ "/Int") (convexHullNum $ genPts @Int n)
--     | i <- [4, 5::Int]
--     , let n = 10^i
--     ] ++
--       -- [ bgroup ("1e"++show i ++ "/SafeDouble") (convexHullFractional $ genPts @SafeDouble n)
--       -- | i <- [4, 5::Int]
--       -- , let n = 10^i
--       -- ] ++
--     [ bgroup ("1e"++show i ++ "/Double") (convexHullFractional $ genPts @Double n)
--     | i <- [4, 5::Int]
--     , let n = 10^i ]
--   where
--     convexHullFractional (force -> pts) =
--                 [ bench "GrahamScan" $ nf GrahamScan.convexHull pts
--                 , bench "DivideAndConquer" $ nf DivideAndConquer.convexHull pts
--                 , bench "QuickHull" $ nf QuickHull.convexHull pts
--                 , bench "JarvisMarch" $ nf JarvisMarch.convexHull pts
--                 ]
--     convexHullNum (force -> pts) = let pts' = force (GrahamV2.fromP <$> pts) in
--                 [ bench "GrahamScan" $ nf GrahamScan.convexHull pts
--                 , bench "GrahamBaseLine" $ nf GrahamV2.convexHull pts'
--                 , bench "DivideAndConquer" $ nf DivideAndConquer.convexHull pts
--                 , bench "JarvisMarch" $ nf JarvisMarch.convexHull pts
--                 ]
--     genPts =
