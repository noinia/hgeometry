module HGeometry.LowerEnvelope.Sample
  ( Probability
  , probability
  , sample, select
  ) where

import Control.Monad.State.Class
import Data.Word
import System.Random.Stateful
import Witherable

--------------------------------------------------------------------------------

-- | A probability a value w represents the probability w/maxBound
type Probability = Word64

-- | Smart constructor to construct a particular probability
probability     :: Int -> Int -> Probability
probability s n = ceiling $ fromIntegral s * r
  where
    r :: Rational
    r = fromIntegral (maxBound @Word64) / fromIntegral n

--------------------------------------------------------------------------------

-- | take a p-sample, by selecting every element with probability p
sample   :: (Witherable t, RandomGen gen, MonadState gen m)
         => Probability
         -> t a -> m (t a)
sample p = filterA (const $ select p)

-- | select a parrticular element with the given probability
select   :: (RandomGen gen, MonadState gen m)
         => Probability
         -> m Bool
select p = (>= p) <$> uniformM StateGenM
