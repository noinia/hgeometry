module Algorithms.Geometry.ConvexHull.Bench where

import qualified Algorithms.Geometry.ConvexHull.DivideAndConquer as DivideAndConquer
import qualified Algorithms.Geometry.ConvexHull.GrahamScan as GrahamScan

-- | copies of the convex hull algo with different point types
import qualified Algorithms.Geometry.ConvexHull.GrahamV2   as GV
import qualified Algorithms.Geometry.ConvexHull.GrahamFam  as GFam
import qualified Algorithms.Geometry.ConvexHull.GrahamFamPeano  as GPeano
import qualified Algorithms.Geometry.ConvexHull.GrahamFam6  as GFam6
import qualified Algorithms.Geometry.ConvexHull.GrahamFixed as GFix


import           Benchmark.Util
import           Control.DeepSeq
import           Criterion.Main
import           Criterion.Types
import           Data.Ext
import           Data.Geometry.Point
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Proxy
import           Test.QuickCheck

--------------------------------------------------------------------------------

main :: IO ()
main = defaultMainWith cfg [ benchmark ]
  where
    cfg = defaultConfig { reportFile = Just "bench.html" }

benchmark :: Benchmark
benchmark = bgroup "convexHullBench"
    [ env (genPts (Proxy :: Proxy Int) 10000) benchBuild
    ]

--------------------------------------------------------------------------------

genPts     :: (Ord r, Arbitrary r) => proxy r -> Int -> IO (NonEmpty (Point 2 r :+ ()))
genPts _ n = generate (NonEmpty.fromList <$> vectorOf n arbitrary)

-- | Benchmark building the convexHull
benchBuild    :: (Ord r, Num r, NFData r) => NonEmpty (Point 2 r :+ ()) -> Benchmark
benchBuild ps = bgroup "build" [ bgroup (show n) (build $ take' n ps)
                               | n <- sizes' ps
                               ]
  where
    take' n = NonEmpty.fromList . NonEmpty.take n
    sizes' _ = [2000]

    build pts = [ bench "sort"                 $ nf NonEmpty.sort pts
                , bench "sort_Linear.V2"       $ nf NonEmpty.sort ptsV2
                , bench "sort_FamPeano"        $ nf NonEmpty.sort ptsFamPeano
                , bench "sort_Family"          $ nf NonEmpty.sort ptsFam
                , bench "sort_Family6"         $ nf NonEmpty.sort ptsFam6
                , bench "sort_Fixed"           $ nf NonEmpty.sort ptsFix

                , bench "grahamScan"           $ nf GrahamScan.convexHull pts
                , bench "grahamScan_Linear.V2" $ nf GV.convexHull         ptsV2
                , bench "grahamScan_FamPeano"  $ nf GPeano.convexHull     ptsFamPeano
                , bench "grahamScan_Family"    $ nf GFam.convexHull       ptsFam
                , bench "grahamScan_Fixed"     $ nf GFix.convexHull       ptsFix

                , bench "Div&Conq"             $ nf DivideAndConquer.convexHull pts
                ]
      where
        ptsV2       = fmap (GV.fromP) pts
        ptsFamPeano = fmap (GPeano.fromP) pts
        ptsFam      = fmap (GFam.fromP) pts
        ptsFam6     = fmap (GFam6.fromP) pts
        ptsFix      = fmap (GFix.fromP) pts
