module ConvexHull.Bench (
    runBenchmark
  , benchmark
  -- , runProfile
  ) where

import           Control.DeepSeq
import           Control.Lens (over)
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified HGeometry.ConvexHull.DivideAndConquer as DivideAndConquer
import qualified HGeometry.ConvexHull.GrahamScan as GrahamScan
import qualified HGeometry.ConvexHull.JarvisMarch as JarvisMarch
import qualified HGeometry.ConvexHull.QuickHull as QuickHull
import           HGeometry.Number.Real.Rational
import           HGeometry.Point
import           System.Random
import           Test.Tasty.Bench
import           Util

--------------------------------------------------------------------------------

type R = RealNumber 5


gen :: StdGen
gen = genByName "convex hull"

--------------------------------------------------------------------------------

runBenchmark :: IO ()
runBenchmark = do
    let intPts = randomPoints @(Point 2 Integer) gen 1000 -- generate points in a 1000x1000 square
    defaultMain [ benchmark intPts
                ]

genPts       :: forall r. (Num r, NFData r)
             => NonEmpty (Point 2 Integer) -> Int -> NonEmpty (Point 2 r)
genPts pts n = pts' `deepseq` (NonEmpty.fromList pts')
  where
    pts' :: [Point 2 r]
    pts' = over coordinates fromInteger <$> NonEmpty.take n pts


-- asPoints   :: [Point 2 Int] -> [point]
-- asPoints n = NonEmpty.fromList . take n

-- | Benchmark building the convexHull
benchmark       :: NonEmpty (Point 2 Integer) -> Benchmark
benchmark inPts = bgroup "ConvexHull" $
      [ bgroup ("1e"++show i ++ "/RealNumber") (convexHullFractional $ genPts @R inPts n)
      | i <- sizes
      , let n = 10^i
      ] ++
      [ bgroup ("1e"++show i ++ "/Int") (convexHullNum $ genPts @Int inPts n)
      | i <- sizes
      , let n = 10^i
      ] ++
      [ bgroup ("1e"++show i ++ "/Double") (convexHullFractional $ genPts @Double inPts n)
      | i <- sizes
      , let n = 10^i
      ]
  where
    sizes :: [Int]
    sizes = [2, 3, 4]

    convexHullFractional pts =
                [ bench "NonEmpty.sort" $ nf NonEmpty.sort pts
                , bench "GrahamScan" $ nf GrahamScan.convexHull pts
                , bench "DivideAndConquer" $ nf DivideAndConquer.convexHull pts
                , bench "QuickHull" $ nf QuickHull.convexHull pts
                , bench "JarvisMarch" $ nf JarvisMarch.convexHull pts
                ]
    convexHullNum pts =
                [ bench "NonEmpty.sort" $ nf NonEmpty.sort pts
                , bench "GrahamScan" $ nf GrahamScan.convexHull pts
                , bench "DivideAndConquer" $ nf DivideAndConquer.convexHull pts
                , bench "JarvisMarch" $ nf JarvisMarch.convexHull pts
                ]
