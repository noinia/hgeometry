module Algorithms.Geometry.LineSegmentIntersection.Bench (benchmark) where

import qualified Algorithms.Geometry.LineSegmentIntersection.BentleyOttmann     as BONew
import qualified Algorithms.Geometry.LineSegmentIntersection.BentleyOttmannNoExt as BONoExt
import qualified Algorithms.Geometry.LineSegmentIntersection.BentleyOttmannOld   as BOOld

import           Control.DeepSeq
import           Control.Lens
import           Control.Monad.Random
import           Data.Ext
import           Geometry.LineSegment
import           Geometry.Point
import           Data.Hashable
import qualified Data.List                 as List
import           Data.RealNumber.Rational
import           Test.Tasty.Bench

--------------------------------------------------------------------------------

type R = RealNumber 5

benchmark :: Benchmark
benchmark = bgroup "LineSegmentIntersection"
    [ benchBuild (evalRand (genPts @R 100) gen)
    ]

gen :: StdGen
gen = mkStdGen (hash "line segment intersection")

--------------------------------------------------------------------------------

genPts                 :: (Ord r, Random r, RandomGen g)
                       => Int -> Rand g [LineSegment 2 () r :+ ()]
genPts n = map ext <$> replicateM n sampleLineSegment

-- | Benchmark computing the closest pair
benchBuild    :: (Ord r, Fractional r, NFData r) => [LineSegment 2 () r :+ ()] -> Benchmark
benchBuild ss = bgroup "LineSegs" [ bgroup (show n) (build $ take n ss)
                                  | n <- sizes' ss
                                  ]
  where
    sizes' xs = [length xs]
      -- let n = length pts in [ n*i `div` 100 | i <- [10,20,25,50,75,100]]

    build segs = [ bench "sort"     $ nf sort' segs
                 , bench "Old"      $ nf BOOld.intersections (map (^.core) segs)
                 , bench "NoExt"    $ nf BONoExt.intersections (map (^.core) segs)
                 , bench "New"      $ nf BONew.intersections segs
                 ]

sort' :: Ord r => [LineSegment 2 () r :+ ()] -> [Point 2 r]
sort' = List.sort . concatMap (\s -> s^..core.endPoints.core)
