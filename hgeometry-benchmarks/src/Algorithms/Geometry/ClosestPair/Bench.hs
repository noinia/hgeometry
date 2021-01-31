module Algorithms.Geometry.ClosestPair.Bench where

import qualified Algorithms.Geometry.ClosestPair.DivideAndConquer as DivideAndConquer
import qualified Algorithms.Geometry.ClosestPair.Naive            as Naive

import           Benchmark.Util
import           Control.DeepSeq
import           Control.Monad.Random
import           Data.Ext
import           Data.Geometry.Point
import           Data.Hashable
import           Data.LSeq            (LSeq)
import qualified Data.LSeq            as LSeq
import           Data.Proxy
import           Test.QuickCheck
import           Test.Tasty.Bench

--------------------------------------------------------------------------------

benchmark :: Benchmark
benchmark = bgroup "convexHullBench"
    [ benchBuild
    ]

--------------------------------------------------------------------------------

genPts                 :: (Ord r, Random r, RandomGen g)
                       => Int -> Rand g (LSeq 2 (Point 2 r :+ ()))
genPts n | n >= 2    = LSeq.promise . LSeq.fromList <$> replicateM n (fmap ext genPoint)
         | otherwise = error "genPts: Need at least 2 points"

genPoint :: (RandomGen g, Random r) => Rand g (Point 2 r)
genPoint = Point2 <$> getRandom <*> getRandom


-- | Benchmark computing the closest pair
benchBuild    :: Benchmark
benchBuild = bgroup "closestPair" [ bgroup (show n) (build $ evalRand (genPts @Int n) gen)
                                  | n <- sizes'
                                  ]
  where
    gen = mkStdGen (hash ("closest pair"::String))
    sizes' = [500]

    build pts = [ bench "sort"     $ nf LSeq.unstableSort pts
                , bench "Div&Conq" $ nf DivideAndConquer.closestPair pts
                , bench "Naive"    $ nf Naive.closestPair pts
                ]
