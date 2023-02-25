{-# LANGUAGE DeriveDataTypeable #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Number.Real.Rational
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--------------------------------------------------------------------------------
module HGeometry.Number.Real.Rational(
    RealNumber(..)

    -- * Converting to and from RealNumber's
  , AsFixed(..), asFixed
  , toFixed, fromFixed
  , Nat
  ) where

import Control.DeepSeq
import Control.Monad.Random
import Data.Aeson
import Data.Data
import Data.Fixed
-- import Data.Hashable
import Data.List (dropWhileEnd)
import Data.Ratio
import GHC.Generics (Generic (..))
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
  deriving (Eq,Ord,Data,Num,Fractional,Real,RealFrac,Generic,ToJSON,FromJSON,NFData)

data NatPrec (p :: Nat) = NatPrec

instance KnownNat p => HasResolution (NatPrec p) where
  resolution _ = 10 ^ (natVal (NatPrec @p))


instance KnownNat p => Show (RealNumber p) where
  showsPrec d r = showParen (d > app_prec && r < 0) $
    case asFixed r of
      Exact p -> showString (dropWhileEnd (== '.') . dropWhileEnd (== '0') . show $ p)
      Lossy p -> shows p . showChar '~'
    where
      app_prec = 10

instance KnownNat p => Read (RealNumber p) where
  readsPrec i = map wrap . readsPrec @(Fixed (NatPrec p)) i
    where
      wrap (RealNumber . realToFrac -> x,s') = case s' of
                                                 '~':s'' -> (x,s'')
                                                 _       -> (x,s')

instance Random (RealNumber p) where
  -- Generate a random number between a and b with 'maxBound `div` 2 :: Int' discrete increments.
  randomR (a,b) = runRand $ do
    v <- getRandom
    pure $ (b-a)*abs v + a
  -- Generate a random number between -1 and +1 with 'maxBound::Int' discrete increments.
  random = runRand $ do
    v <- getRandom
    let fromInt :: Int -> Integer; fromInt = fromIntegral
    pure $ RealNumber $ fromInt v % fromInt maxBound

--------------------------------------------------------------------------------



-- | Fixed-precision representation of a 'RealNumber'. If there's insufficient
--   precision to accurately represent the 'RealNumber' then the 'Lossy' constructor
--   will be used.
data AsFixed p = Exact !(Fixed p) | Lossy !(Fixed p) deriving (Show,Eq)

-- | Cast 'RealNumber' to a fixed-precision number. Data is silently lost if there's
--   insufficient precision.
toFixed :: KnownNat p => RealNumber p -> Fixed (NatPrec p)
toFixed = realToFrac

-- | Cast a fixed-precision number to a 'RealNumber'.
fromFixed :: KnownNat p => Fixed (NatPrec p) -> RealNumber p
fromFixed = realToFrac

-- | Cast 'RealNumber' to a fixed-precision number. Data-loss caused by insufficient
--   precision will be marked by the 'Lossy' constructor.
asFixed   :: KnownNat p => RealNumber p -> AsFixed (NatPrec p)
asFixed r = let p = toFixed r in if r == fromFixed p then Exact p else Lossy p
