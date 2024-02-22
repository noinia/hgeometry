module ConvexHull.GrahamBench (
    runBenchmark
  , benchmark
  ) where


import qualified HGeometry.ConvexHull.GrahamScan as GrahamScan
import qualified ConvexHull.GrahamV2 as GrahamV2
import qualified ConvexHull.GrahamInt as GrahamInt
import qualified ConvexHull.GrahamFastest as GrahamFastest
  -- hand written implementation for Int, this should be the fastest possible somehow.

import           Control.DeepSeq
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Vector.Unboxed as UV
import           HGeometry.Foldable.Sort
import           HGeometry.Point
import           HGeometry.Polygon.Convex
import           System.Random
import           Test.Tasty.Bench
import           Util

--------------------------------------------------------------------------------

gen :: StdGen
gen = genByName "graham scan bench"

-- runProfile :: IO ()
-- runProfile = do
--     let pts = take' (10^(4 :: Int)) $ randomPoints @(Point 2 Int) gen 100000
--     let pg = force $ GrahamScan.convexHull pts
--     print (lengthOf outerBoundary pg)

--------------------------------------------------------------------------------

{-
the -fspecialise-aggressively -fexpose-all-unfoldings flags are
curcial in making sure this benchmark is sufficiently fast.
-}

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
runBenchmark = defaultMain [ benchmark ]

benchmark :: Benchmark
benchmark = chBench $ take' 1_000 $ randomPoints @(Point 2 Int) gen 1000
  where
    chBench pts = bgroup "ConvexHull.GrahamBench"
        [ bench "GrahamScan"    $ nf GrahamScan.convexHull pts
        , bench "GrahamScanV2"  $ nf GrahamV2.convexHull   (GrahamV2.fromP <$> pts)
        , bench "GrahamScanInt" $ nf GrahamInt.convexHull (GrahamInt.fromP <$> pts)
        , bench "GrahamScanFastest" $ nf GrahamFastest.convexHull (GrahamFastest.fromP <$> pts)
        --   -- , bench "GrahamScanClassy" $ nf GrahamClassy.convexHull (GrahamClassy.fromP <$> pt                                                                           s)

        --   -- , bench "ClassySort" $ nf GrahamClassy.sort' (GrahamClassy.fromP <$> pts)
        -- , bench "Sort" $ nf sort' pts
        ]
      -- where
      --   pts' = force ((\(Point2_ x y) -> Point2 (fromIntegral x) (fromIntegral y)) <$> pts)
