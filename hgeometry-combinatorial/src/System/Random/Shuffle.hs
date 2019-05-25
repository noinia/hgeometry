--------------------------------------------------------------------------------
-- |
-- Module      :  System.Random.Shuffle
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Implements Fishyer-Yates shuffle.
--
--------------------------------------------------------------------------------
module System.Random.Shuffle(shuffle) where

import           Control.Monad
import           Control.Monad.Random.Class
import qualified Data.Foldable as F
import           Data.Util
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

--------------------------------------------------------------------------------

-- | Fisher–Yates shuffle, which shuffles a list/foldable uniformly at random.
--
-- running time: \(O(n)\).
shuffle :: (Foldable f, MonadRandom m) => f a -> m (V.Vector a)
shuffle = withLength . V.fromList . F.toList
  where
    withLength v = let n = V.length v in flip withRands v <$> rands (n - 1)
    withRands rs = V.modify $ \v ->
                     forM_ rs $ \(SP i j) -> MV.swap v i j


-- | Generate a list of indices in decreasing order, coupled with a random
-- value in the range [0,i].
rands   :: MonadRandom m => Int -> m [SP Int Int]
rands n = mapM (\i -> SP i <$> getRandomR (0,i)) [n,(n-1)..1]
