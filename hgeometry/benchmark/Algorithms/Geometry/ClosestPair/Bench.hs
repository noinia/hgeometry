module Algorithms.Geometry.ClosestPair.Bench where

import qualified Algorithms.Geometry.ClosestPair.DivideAndConquer as DivideAndConquer
import qualified Algorithms.Geometry.ClosestPair.Naive as Naive
import           Benchmark.Util
import           Control.DeepSeq
import           Criterion.Main
import           Criterion.Types
import           Data.Ext
import           Data.Geometry.Point
import           Data.Proxy
import           Test.QuickCheck
import           Data.LSeq (LSeq)
import qualified Data.LSeq as LSeq

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

genPts                 :: (Ord r, Arbitrary r)
                       => proxy r -> Int -> IO (LSeq 2 (Point 2 r :+ ()))
genPts _ n | n >= 2    = generate (LSeq.promise . LSeq.fromList <$> vectorOf n arbitrary)
           | otherwise = error "genPts: Need at least 2 points"

-- | Benchmark computing the closest pair
benchBuild    :: (Ord r, Num r, NFData r) => LSeq 2 (Point 2 r :+ ()) -> Benchmark
benchBuild ps = bgroup "closestPair" [ bgroup (show n) (build $ take' n ps)
                                     | n <- sizes' ps
                                     ]
  where
    take' n = LSeq.promise . LSeq.take n
    sizes' pts = let n = length pts in [ n*i `div` 100 | i <- [10,20,25,50,75,100]]

    build pts = [ bench "sort"     $ nf LSeq.unstableSort pts
                , bench "Div&Conq" $ nf DivideAndConquer.closestPair pts
                , bench "Naive"    $ nf Naive.closestPair pts
                ]
