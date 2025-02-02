{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Main( main ) where

import           Control.DeepSeq
import           Control.Monad (replicateM)
import           Data.Foldable
import qualified Data.IntSet as IntSet
import qualified Data.List as List
import           Data.Ord (comparing)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import           HGeometry.Permutation.Shuffle
import           System.Random
import           System.Random.Stateful
import           Test.Tasty.Bench

--------------------------------------------------------------------------------

-- n = 100_000
n = 100_000

main :: IO ()
main = do
  (myInts :: [Int]) <- force <$> replicateM n (uniformM globalStdGen)
  seed <- uniformM globalStdGen
  let gen = mkStdGen seed

  -- print $ shuffleSeqInOut gen myInts

  defaultMain
    [ bgroup "shuffle tests"
      [ bench "mutable shuffle"           $ nf (shuffle @V.Vector gen) myInts
      , bench "mutable shuffle unboxed"   $ nf (shuffle @U.Vector gen) myInts
      , bench "shuffle sequence"          $ nf (shuffleSeq gen)        myInts
      , bench "shuffle sequence (in out orig)" $ nf (shuffleSeqInOutOrig gen)   myInts
      , bench "shuffle sequence (in out)" $ nf (shuffleSeqInOut gen)   myInts
      , bench "shuffle intmap"            $ nf (shuffleIntMap gen)       myInts
      , bench "unsafe shuffle sort"       $ nf (unsafeShuffleBySort gen) myInts
      -- , bench "shuffle sort"              $ nf (shuffleBySort gen)       myInts
      ]
    ]


--------------------------------------------------------------------------------
-- Some little experiment; generating unchecked random indices and then sorting them on
-- index. This is slower than what we did using the Seq's though.


-- | Version that doesn't check whether we have dupliates
-- note: this only works if n <= max int size
unsafeShuffleBySort         :: (RandomGen gen, Foldable f) => gen -> f a -> [a]
unsafeShuffleBySort gen0 xs = map snd
                            . List.sortBy (comparing fst)
                            . fst . foldr (\x (res,gen) ->
                                             let (i,gen') = uniform @_ @Int gen
                                             in ((i,x):res,gen')
                                          ) ([],gen0)
                            $ xs


shuffleBySort         :: (RandomGen gen, Foldable f) => gen -> f a -> [a]
shuffleBySort gen0 xs = let m = length xs
                        in map snd
                         . List.sortBy (comparing fst)
                         $ zip (constructIndices m gen0) (toList xs)

                         -- . fst . foldr (\x (res,gen) ->
                         --                  let (i,gen') = uniformR (0,n*n) gen
                         --                  in ((i,x):res,gen)
                         --               ) ([],gen0)
                         -- $ xs


constructIndices   :: RandomGen gen => Int -> gen -> [Int]
constructIndices m = go mempty
  where
    go s gen | IntSet.size s == m = []
             | otherwise          = case uniform gen of
                         (x,gen') | IntSet.member x s -> go s gen'
                                  | otherwise         -> x : go (IntSet.insert x s) gen'
