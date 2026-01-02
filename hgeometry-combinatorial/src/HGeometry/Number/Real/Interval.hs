--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Number.Real.Interval
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Interval Arithmetic. We use doubles as initial approximation of the interval.
--------------------------------------------------------------------------------

module HGeometry.Number.Real.Interval
  ( IntervalReal
  , exactValue
  , fromExact
  ) where

import Numeric.Rounded.Hardware
import Numeric.Rounded.Hardware.Interval.Class
import Numeric.Rounded.Hardware.Interval.NonEmpty
import GHC.Generics (Generic)
import Control.DeepSeq
import Data.Bifunctor
import Control.Monad.Random
import Text.Read
--------------------------------------------------------------------------------


data IntervalReal r = IR {-# UNPACK#-}!(Interval Double) r
  deriving (Generic)

-- | Get the exact vavlue of the interval-real.
exactValue          :: IntervalReal r -> r
exactValue (IR _ x) = x

-- | Construct an intervalReal from a given r value.
fromExact   :: Real r => r -> IntervalReal r
fromExact x = IR (realToFrac x) x


instance NFData r => NFData (IntervalReal r)

instance Show r => Show (IntervalReal r) where
  showsPrec d (IR _ r) = showsPrec d r

instance (Read r, Real r) => Read (IntervalReal r) where
  readPrec = fromExact <$> readPrec

instance Eq r => Eq (IntervalReal r)  where
  (IR ix x) == (IR iy y) = not (disjoint ix iy) && x == y
         -- the disjoint test is important; only if the intervals are not disjoint check
         -- for actual equality

instance Ord r => Ord (IntervalReal r)  where
  (IR ix x) `compare` (IR iy y)
    | ix `strictPrecedes` iy = LT
    | iy `strictPrecedes` ix = GT
    | otherwise              = x `compare` y
    -- we could maybe do something more interesting here

instance (Num r, Ord r) => Num (IntervalReal r) where
  (IR ix x) + (IR iy y) = IR (ix + iy) (x + y)
  (IR ix x) - (IR iy y) = IR (ix - iy) (x - y)
  (IR ix x) * (IR iy y) = IR (ix * iy) (x * y)
  abs (IR ix x)    = IR (abs ix) (abs x)
  signum (IR (I l u) x)
    | 0 < getRounded l = 1
    | 0 > getRounded u = -1
    | otherwise        = case 0 `compare` x of
                           LT -> 1
                           EQ -> 0
                           GT -> (-1)
  fromInteger i    = IR (fromInteger i) (fromInteger i)
  negate (IR ix x) = IR (negate ix) (negate x)

instance (Fractional r, Ord r) => Fractional (IntervalReal r) where
  fromRational x = IR (fromRational x) (fromRational x)
  (IR ix x) / (IR iy y) = IR (ix / iy) (x / y)

-- FIXME: we are potentially dividing by 0 apparently

instance Real r => Real (IntervalReal r) where
  toRational (IR _ x) = toRational x


-- instance Uniform r => Uniform (IntervalReal r) where

instance (Random r, Real r) => Random (IntervalReal r) where
  randomR (a,b) = first fromExact . randomR (exactValue a, exactValue b)
  random = first fromExact . random


  --   runRand $ do
  --   v <- getRandom
  --   pure $ (b-a)*abs v + a
  -- -- Generate a random number between -1 and +1 with 'maxBound::Int' discrete increments.
  -- random = runRand $ do
  --   v <- getRandom
  --   let fromInt :: Int -> Integer; fromInt = fromIntegral
  --   pure $ RealNumber $ fromInt v % fromInt maxBound
