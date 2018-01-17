module Algorithms.Geometry.ConvexHullBench where

import qualified Algorithms.Geometry.ConvexHull.DivideAndConqueror as DivideAndConqueror
import qualified Algorithms.Geometry.ConvexHull.GrahamScan as GrahamScan

import qualified Algorithms.Geometry.Graham as GS
import qualified Algorithms.Geometry.GrahamFam as GF

import           Benchmark.Util
import           Control.DeepSeq
import           Control.Lens
import           Criterion.Main
import           Criterion.Types
import           Data.Ext
import           Data.Geometry.Point
import           Data.Geometry.Polygon
import           Data.Geometry.Polygon.Convex
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Proxy
import           Test.QuickCheck
import           Test.QuickCheck.HGeometryInstances ()

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
                               | n <- sizes ps
                               ]
  where
    take' n = NonEmpty.fromList . NonEmpty.take n

    build pts = [ bench "sort"               $ nf NonEmpty.sort pts'
                , bench "grahamScan"         $ nf GrahamScan.convexHull pts
                , bench "grahamScanWithoutP" $ nf GS.convexHull pts'
                , bench "grahamScanFamily"   $ nf GF.convexHull pts''

                --, bench "Div&Conq"   $ nf DivideAndConqueror.convexHull pts
                ]
      where
        pts'  = fmap (GS.fromP) pts
        pts'' = fmap (GF.fromP) pts
