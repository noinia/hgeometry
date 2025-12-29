module HGeometry.Number.Real.Interval
  ( IntervalReal
  ) where

-- import Numeric.Rounded.Hardware
import Numeric.Rounded.Hardware.Interval.NonEmpty
import GHC.Generics(Generic)
import Control.DeepSeq
--------------------------------------------------------------------------------


data IntervalReal r = IR {-# UNPACK#-}!(Interval Double) r
  deriving (Generic)

instance NFData r => NFData (IntervalReal r)

instance Show r => Show (IntervalReal r) where
  showsPrec d (IR _ r) = showsPrec d r



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

instance Num r => Num (IntervalReal r) where
  (IR ix x) + (IR iy y) = IR (ix + iy) (x + y)
  (IR ix x) - (IR iy y) = IR (ix - iy) (x - y)
  (IR ix x) * (IR iy y) = IR (ix * iy) (x * y)
  abs (IR ix x)    = IR (abs ix) (abs x)
  signum (IR (Interval l u) x)
    | 0 < getRounded l = 1
    | 0 > getRounded u = -1
    | otherwise        = signum x
  fromInteger i    = IR (fromInteger i) (fromInteger i)
  negate (IR ix x) = IR (negate ix) (negate x)

instance Fractional r => Fractional (IntervalReal r) where
  fromRational x = IR (fromRational x) (fromRational x)
  (IR ix x) / (IR iy y) = IR (ix / iy) (x / y)
