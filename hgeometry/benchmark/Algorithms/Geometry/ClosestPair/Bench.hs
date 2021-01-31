module Algorithms.Geometry.ClosestPair.Bench where

import qualified Algorithms.Geometry.ClosestPair.DivideAndConquer as DivideAndConquer
import qualified Algorithms.Geometry.ClosestPair.Naive            as Naive

import           Control.Monad.Random
import           Data.Ext
import           Data.Geometry.Point
import           Data.Hashable
import           Data.LSeq            (LSeq)
import qualified Data.LSeq            as LSeq
import           Test.Tasty.Bench

--------------------------------------------------------------------------------

genPts                 :: (Ord r, Random r, RandomGen g)
                       => Int -> Rand g (LSeq 2 (Point 2 r :+ ()))
genPts n | n >= 2    = LSeq.promise . LSeq.fromList <$> replicateM n (fmap ext getRandom)
         | otherwise = error "genPts: Need at least 2 points"

gen :: StdGen
gen = mkStdGen (hash "closest pair")

-- | Benchmark computing the closest pair
benchmark    :: Benchmark
benchmark = bgroup "ClosestPair"
    [ bgroup (show n) (build $ evalRand (genPts @Int n) gen)
    | n <- sizes'
    ]
  where
    sizes' = [500]

    build pts = [ bench "sort"     $ nf LSeq.unstableSort pts
                , bench "Div&Conq" $ nf DivideAndConquer.closestPair pts
                , bench "Naive"    $ nf Naive.closestPair pts
                ]
