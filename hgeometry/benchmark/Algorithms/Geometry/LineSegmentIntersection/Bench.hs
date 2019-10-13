module Algorithms.Geometry.LineSegmentIntersection.Bench where

import qualified Algorithms.Geometry.LineSegmentIntersection.BentleyOttmann as BONew
import qualified Algorithms.Geometry.LineSegmentIntersection.BentleyOttmannOld as BOOld
import           Algorithms.Geometry.LineSegmentIntersection.Types
import           Benchmark.Util
import           Control.DeepSeq
import           Control.Lens
import           Criterion.Main
import           Criterion.Types
import           Data.Ext
import           Data.Geometry.LineSegment
import           Data.Geometry.Point
import qualified Data.LSeq as LSeq
import qualified Data.List as List
import           Data.Proxy
import           Test.QuickCheck

--------------------------------------------------------------------------------

main :: IO ()
main = defaultMainWith cfg [ benchmark ]
  where
    cfg = defaultConfig { reportFile = Just "bench.html" }

benchmark :: Benchmark
benchmark = bgroup "linesegmentIntersectionBench"
    [ env (genPts (Proxy :: Proxy Rational) 100) benchBuild
    ]

--------------------------------------------------------------------------------

genPts                 :: (Ord r, Arbitrary r)
                       => proxy r -> Int
                       -> IO [LineSegment 2 () r]
genPts _ n | n >= 2    = generate (vectorOf n arbitrary)
           | otherwise = error "genPts: Need at least 2 points"

-- | Benchmark computing the closest pair
benchBuild    :: (Ord r, Fractional r, NFData r) => [LineSegment 2 () r] -> Benchmark
benchBuild ss = bgroup "LineSegs" [ bgroup (show n) (build $ take n ss)
                                  | n <- sizes' ss
                                  ]
  where
    sizes' xs = [length xs]
      -- let n = length pts in [ n*i `div` 100 | i <- [10,20,25,50,75,100]]

    build segs = [ bench "sort"     $ nf sort' segs
                 , bench "Old"      $ nf BOOld.intersections segs
                 , bench "New"      $ nf BONew.intersections segs
                 ]

sort' :: Ord r => [LineSegment 2 () r] -> [Point 2 r]
sort' = List.sort . concatMap (\s -> s^..endPoints.core)
