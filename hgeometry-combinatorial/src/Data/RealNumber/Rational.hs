{-# LANGUAGE DeriveDataTypeable #-}
module Data.RealNumber.Rational(RealNumber(..)) where

import Data.Data
import Data.Fixed
import Data.List (dropWhileEnd)
import GHC.Generics (Generic(..))
import GHC.TypeLits

--------------------------------------------------------------------------------

-- | Real Numbers represented using Rational numbers. The number type
-- itself is exact in the sense that we can represent any rational
-- number.
--
-- The parameter, a natural number, represents the precision (in
-- number of decimals behind the period) with which we display the
-- numbers when printing them (using Show).
--
-- If the number cannot be displayed exactly a '~' is printed after
-- the number.
newtype RealNumber (p :: Nat) = RealNumber Rational
  deriving (Eq,Ord,Data,Num,Fractional,Real,RealFrac,Generic)


data NatPrec (p :: Nat) = NatPrec

instance KnownNat p => HasResolution (NatPrec p) where
  resolution _ = 10 ^ (1 + natVal (NatPrec @p))


instance KnownNat p => Show (RealNumber p) where
  showsPrec i (RealNumber r) = case asFixed @(NatPrec p) r of
                                 Exact p -> fmap (dropWhileEnd (== '0')) . showsPrec i $ p
                                 Lossy p -> fmap (<> "~") . showsPrec i $ p

instance KnownNat p => Read (RealNumber p) where
  readsPrec i = map wrap . readsPrec @(Fixed (NatPrec p)) i
    where
      wrap (RealNumber . realToFrac -> x,s') = case s' of
                                                 '~':s'' -> (x,s'')
                                                 _       -> (x,s')


data AsFixed p = Exact !(Fixed p) | Lossy !(Fixed p) deriving (Show,Eq)

toFixed :: HasResolution p => Rational -> Fixed p
toFixed = realToFrac

fromFixed :: HasResolution p => Fixed p -> Rational
fromFixed = realToFrac

asFixed   :: HasResolution p => Rational -> AsFixed p
asFixed r = let p = toFixed r in if r == fromFixed p then Exact p else Lossy p
