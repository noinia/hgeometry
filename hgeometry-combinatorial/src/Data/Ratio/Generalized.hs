--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Ratio.Generalized
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Generalized Ratio type that accepts arbitrary 'Num a' types rather
-- than just Integral ones as in Data.Ratio
--------------------------------------------------------------------------------
module Data.Ratio.Generalized
  ( GRatio
  , (%)
  , numerator, denominator
  ) where

import qualified GHC.Real as Ratio
import           Test.QuickCheck (Arbitrary(..), suchThat)
import           Test.QuickCheck.Instances ()

--------------------------------------------------------------------------------

-- | Generalized Ratio type that accepts more general "base" types
-- than just Integral ones. That does mean we cannot normalize the
-- intermediate expressions, so expect the numbers to become big quite
-- quickly!
--
-- invariant: the denominator is not zero
data GRatio a = !a :% !a
              deriving (Show)

-- | Get the numerator
numerator          :: GRatio a -> a
numerator (a :% _) = a

-- | Get the denominator
denominator          :: GRatio a -> a
denominator (_ :% b) = b

-- | smart constructor to construct a GRatio. Throws an exception if
-- the denominator is zero.
(%)   :: (Eq a, Num a) => a -> a -> GRatio a
_ % 0 = Ratio.ratioZeroDenominatorError
a % b = a :% b

instance (Eq a, Num a) => Eq (GRatio a) where
  (a :% b) == (c :% d) = a*d == b*c -- by invariant b and d are non-zero
  {-# INLINABLE (==) #-}

instance (Ord a, Num a) => Ord (GRatio a) where
  (a :% b) `compare` (c :% d)
    | signum b == signum d = (a*d) `compare` (b*c) -- by invariant b and d are non-zero
    | otherwise            = (b*c) `compare` (a*d) -- by invariant b and d are non-zero

instance (Num a, Eq a) => Num (GRatio a) where
  (a :% b) + (c :% d) = (a*d + b*c) :% (b*d)
  -- since b and d where non-zero, b*d is also non-zero
  negate (a :% b) = negate a :% b
  -- b was non-zero, it remains non-zero
  (a :% b) * (c :% d) = (a*c) :% (b*d)
  -- since b and d where non-zero, b*d is also non-zero
  fromInteger x = fromInteger x :% 1
  signum (a :% b) = (signum a * signum b) :% 1
  -- by invariant b cannot be zero, so signum b cannot be zero either.
  abs x | signum x == -1 = (-1)*x
        | otherwise      = x

instance (Num a, Eq a) => Fractional (GRatio a) where
  fromRational (a Ratio.:% b)= fromInteger a :% fromInteger b
  (a :% b) / (c :% d) = (a*d) % (b*c)
    -- b is non-zero, but c may be zero, so in that case we would be
    -- dividing by zero.  however, if c is zero then (c % d) would be
    -- zero, so there is no need to explicitly handle that; i.e. that
    -- is a something the user must do

instance (Arbitrary a, Num a, Eq a) => Arbitrary (GRatio a) where
  arbitrary = (:%) <$> arbitrary <*> (arbitrary `suchThat` (/= 0))
