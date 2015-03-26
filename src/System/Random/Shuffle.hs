module System.Random.Shuffle where

import           Control.Monad
import           System.Random

import qualified Data.Vector.Mutable as MV
import qualified Data.Vector         as V



-- | Fisher–Yates shuffle, which shuffles in O(n) time.
shuffle     :: RandomGen g => g -> [a] -> [a]
shuffle gen = V.toList . V.modify (\v ->
                do
                  let n = MV.length v
                  forM_ (rands gen $ n - 1) $ \(i,j) -> MV.swap v i j
              ) . V.fromList
  where
    -- rands     :: RandomGen g => Int -> [(Int,Int)]
    rands g i
      | i <= 0    = []
      | otherwise = let (j,g') = randomR (0,i) g in (i,j) : rands g' (i-1)
