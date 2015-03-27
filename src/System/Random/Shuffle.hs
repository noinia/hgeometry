module System.Random.Shuffle(shuffle) where

import           Control.Monad
import qualified Data.Foldable as F
import           System.Random

import qualified Data.Vector.Mutable as MV
import qualified Data.Vector         as V



-- | Fisher–Yates shuffle, which shuffles in O(n) time.
shuffle     :: (F.Foldable f, RandomGen g) => g -> f a -> [a]
shuffle gen = V.toList . V.modify (\v ->
                do
                  let n = MV.length v
                  forM_ (rands gen $ n - 1) $ \(SP i j) -> MV.swap v i j
              ) . V.fromList . F.toList

-- | Strict pair
data SP a b = SP !a !a

-- | Generate a list of indices in decreasing order, coupled with a random
-- value in the range [0,i].
rands     :: RandomGen g => g -> Int -> [SP Int Int]
rands g i
      | i <= 0    = []
      | otherwise = let (j,g') = randomR (0,i) g in SP i j : rands g' (i-1)
