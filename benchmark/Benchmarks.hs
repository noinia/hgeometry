module Main where

import           Control.DeepSeq
import           Control.Lens
import           Criterion.Main
import           Data.Ext
import           Data.Geometry.Interval
import qualified Data.Geometry.IntervalTree as IT
import           Data.Geometry.SegmentTree (I(..))
import qualified Data.Geometry.SegmentTree as SegTree
import qualified Data.List.NonEmpty as NonEmpty
import           QuickCheck.Instances
import           Test.QuickCheck


import WSPDBench

-- | generates n random intervals
genIntervals                  :: (Ord r, Arbitrary r)
                              => proxy r -> Int -> IO [Interval () r]
genIntervals _ n | n <= 0     = error "genIntervals: need n > 0"
                 | otherwise  = generate (vectorOf n arbitrary)

genQueries                      :: (Ord r, Arbitrary r)
                                => proxy r -> Int -> IO [r]
genQueries _ n | n <= 0     = error "genQueries: need n > 0"
               | otherwise  = generate (vectorOf n arbitrary)


-- genQuerySetup     :: (Ord r, Arbitrary r)
--                   => proxy r -> Int -> IO (Int,IT.IntervalTree (I (Interval () r)) r, [r])
-- genQuerySetup p n = (\is qs -> (n, IT.fromIntervals . fmap I $ is, qs))
--                  <$> genIntervals p n
--                  <*> genQueries   p n



main = defaultMain [ benchWSPD ]

intervalBench = bgroup "IntervalTree"
    [ env (genIntervals (I (5 :: Int)) (100000 :: Int)) benchBuild
      -- env (genIntervals (I (5 :: Int)) (100000 :: Int)) benchQueryIT
    ]


benchBuild    :: (Ord r, NFData r) => [Interval () r] -> Benchmark
benchBuild is = bgroup "build" [ bench (show n) $ nf IT.fromIntervals (take n is')
                               | n <- sizes is
                               ]
  where
    is' = I <$> is

benchQueryIT    :: (Ord r, Arbitrary r, NFData r) => [Interval () r] -> Benchmark
benchQueryIT is = bgroup "queries"
    [ env (setup n) (\(t,qs) ->
                        bench ("queries on size" ++ show n) $ whnf (queryAll t) qs)
    | n <- sizes is
    ]
  where
    is'        = I <$> is
    r          = is^.to head.start.core
    setup n    = (IT.fromIntervals (take n is'),) <$> genQueries (I r) 100000
    queryAll t = map (flip IT.search t)


-- benchQueryIT          :: Ord r
--                       => (Int, IT.IntervalTree (I (Interval () r)) r, [r]) -> Benchmark
-- benchQueryIT (n,t,qs) = bgroup "queries" [ bench "query" $ whnf (flip IT.search t) q
--                                          | q <- qs
--                                          ]


sizes    :: [a] -> [Int]
sizes xs = let n = length xs in (\i -> n*i `div` 100) <$> [5,10..100]
