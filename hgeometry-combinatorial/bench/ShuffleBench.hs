module Main( main ) where

import           Control.DeepSeq
import           Control.Monad (replicateM)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import           HGeometry.Permutation.Shuffle
import           System.Random
import           System.Random.Stateful
import           Test.Tasty.Bench

--------------------------------------------------------------------------------

n = 100_000

main :: IO ()
main = do
  (myInts :: [Int]) <- force <$> replicateM n (uniformM globalStdGen)
  seed <- uniformM globalStdGen
  let gen = mkStdGen seed
  defaultMain
    [ bgroup "shuffle tests"
      [ bench "mutable shuffle"           $ nf (shuffle @V.Vector gen) myInts
      , bench "mutable shuffle unboxed"   $ nf (shuffle @U.Vector gen) myInts
      , bench "shuffle sequence"          $ nf (shuffleSeq gen)        myInts
      , bench "shuffle sequence (in out orig)" $ nf (shuffleSeqInOutOrig gen)   myInts
      , bench "shuffle sequence (in out)" $ nf (shuffleSeqInOut gen)   myInts
      ]
    ]
