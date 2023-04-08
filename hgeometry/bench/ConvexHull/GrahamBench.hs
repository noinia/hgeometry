module ConvexHull.GrahamBench (
    runBenchmark
  , benchmark
  , runProfile
  ) where


import qualified HGeometry.ConvexHull.GrahamScan as GrahamScan
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
gen = genByName "graham scan bench"

runProfile :: IO ()
runProfile = do
    let pts = take' (10^(4 :: Int)) $ randomPoints @(Point 2 Int) gen 100000
    let pg = force $ GrahamScan.convexHull pts
    print (lengthOf outerBoundary pg)

--------------------------------------------------------------------------------

{-
the -fspecialise-aggressively -fexpose-all-unfoldings flags are
curcial in making sure this benchmark is sufficiently fast.
-}


-- myConvexHull :: NonEmpty      (PointF (VectorFamily 2 Int))
--              -> ConvexPolygon (PointF (VectorFamily 2 Int))
-- myConvexHull = GrahamScan.convexHull

myConvexHull :: NonEmpty      (Point 2 Double)
             -> ConvexPolygon (Point 2 Double)
myConvexHull = GrahamScan.convexHull


sort' :: NonEmpty      (Point 2 Int) ->
         NonEmpty      (Point 2 Int)
sort' = NonEmpty.fromList . UV.toList . sortBy incXdecY

incXdecY :: (Ord r, Point_ point 2 r) => point -> point -> Ordering
incXdecY (Point2_ px py) (Point2_ qx qy) =
  compare px qx <> compare qy py

-- this already seems to make a difference versus just using Point 2 Int:

    -- 1e4Int
    --   GrahamScan:        OK (0.80s)
    --     112  ms ± 1.8 ms
    --   GrahamScanAnnot:   OK (0.59s)
    --     83.2 ms ± 6.1 ms

-- still a far cry off from the

      -- GrahamScanFastest: OK (0.32s)
      --   2.33 ms ± 125 μs

-- though
--------------------------------------------------------------------------------


runBenchmark :: IO ()
runBenchmark = do
    let intPts = randomPoints @(Point 2 Int) gen 100000

    defaultMain [ benchmark "Int" [3..5] chBench intPts
                ]
  where
    chBench     :: NonEmpty (Point 2 Int) -> [Benchmark]
    chBench pts = [ bench "GrahamScan"    $ nf GrahamScan.convexHull pts
                  , bench "GrahamScanAnnot"    $ nf myConvexHull pts'
                  , bench "GrahamScanV2"  $ nf GrahamV2.convexHull   (GrahamV2.fromP <$> pts)
                  , bench "GrahamScanInt" $ nf GrahamInt.convexHull (GrahamInt.fromP <$> pts)
                  , bench "GrahamScanFastest" $ nf GrahamFastest.convexHull (GrahamFastest.fromP <$> pts)
                  -- , bench "GrahamScanClassy" $ nf GrahamClassy.convexHull (GrahamClassy.fromP <$> pt                                                                           s)

                  -- , bench "ClassySort" $ nf GrahamClassy.sort' (GrahamClassy.fromP <$> pts)
                  , bench "Sort" $ nf sort' pts
                  ]
      where
        pts' = force ((\(Point2_ x y) -> Point2 (fromIntegral x) (fromIntegral y)) <$> pts)



-- | Benchmark building the convexHull
benchmark                  :: NFData point
                           => String -- ^ name
                           -> [Int] -- ^ sizes i s.t. size is actually 10^i
                           -> (NonEmpty point -> [Benchmark])  -- ^ function to benchmark
                           -> NonEmpty point -- ^ sufficiently log list of points
                           -> Benchmark
benchmark name sizes f pts = bgroup "ConvexHull" $
    [ bgroup ("1e" <> show i <> name) (f . force $ take' n pts)
    | i <- sizes
    , let n = 10^i
    ]
