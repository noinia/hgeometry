{-# LANGUAGE GADTs               #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | See: https://randomascii.wordpress.com/2012/02/25/comparing-floating-point-numbers-2012-edition/
module Data.Double.Approximate
  ( SafeDouble
  , DoubleRelAbs(..)
  ) where

import Data.Proxy
import GHC.TypeLits
import Numeric.MathFunctions.Comparison
import Numeric.MathFunctions.Constants
import Text.Read

-- | Relatively safe double floating-point type with a relative error
--   margin of 10 ULPs and an absolute margin around zero of 10*epsilon.
--
--   Warning: All numbers within 10*epsilon of zero will be considered zero.
--
-- >>> m_epsilon * 10
-- 2.220446049250313e-15
--
-- >>> realToFrac (m_epsilon * 10) == (0::SafeDouble)
-- False
--
-- >>> realToFrac (m_epsilon * 9) == (0::SafeDouble)
-- True
--
-- >>> 1e-20 == (5e-20 :: Double)
-- False
-- >>> 1e-20 == (5e-20 :: SafeDouble)
-- True
--
-- 'pi' and 'sin' are approximations:
-- >>> sin pi
-- 1.2246467991473532e-16
-- >>> sin pi == (0 :: Double)
-- False
-- >>> sin pi == (0 :: SafeDouble)
-- True
--
type SafeDouble = DoubleRelAbs 10 10

-- | Custom double floating-point type with a relative error margin of
--   'rel' number of ULPs and an absolute error margin of 'abs' times
--   epsilon.
--
--   The relative error margin is the primary tool for good numerical
--   robustness and can relatively safely be set to a high number such
--   as 100. The absolute error margin is a last ditch attempt at fixing
--   broken algorithms and dramatically limits the resolution around zero.
--   If possible, use a low absolute error margin.
newtype DoubleRelAbs (abs :: Nat) (rel :: Nat) = DoubleRelAbs Double
  deriving (Num, Enum, Floating, Fractional, Real, RealFloat, RealFrac)

instance (KnownNat abs, KnownNat rel) => Eq (DoubleRelAbs abs rel) where
  DoubleRelAbs d1 == DoubleRelAbs d2 =
    within (fromIntegral (natVal @rel Proxy)) d1 d2 ||
    (abs d1 < m_epsilon * fromIntegral (natVal @abs Proxy) &&
     abs d2 < m_epsilon * fromIntegral (natVal @abs Proxy))

instance (KnownNat abs, KnownNat rel) => Ord (DoubleRelAbs abs rel) where
  DoubleRelAbs d1 `compare` DoubleRelAbs d2
    | within (fromIntegral (natVal @rel Proxy)) d1 d2 = EQ
    | otherwise = d1 `compare` d2

instance Show (DoubleRelAbs abs rel) where
  showsPrec i (DoubleRelAbs d) = showsPrec i d

instance Read (DoubleRelAbs abs rel) where
  readPrec = DoubleRelAbs <$> readPrec

