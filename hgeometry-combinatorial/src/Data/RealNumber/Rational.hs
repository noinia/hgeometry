{-# LANGUAGE DeriveDataTypeable #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Data.RealNumber.Rational
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--------------------------------------------------------------------------------
module Data.RealNumber.Rational(RealNumber(..)

                               -- * Converting to and from RealNumber's
                               , AsFixed(..), asFixed
                               , toFixed, fromFixed
                               , Nat
                               ) where

import Data.Aeson
import Data.Data
import Data.Fixed
import Data.Hashable
import Data.List       (dropWhileEnd)
import GHC.Generics    (Generic (..))
import GHC.TypeLits
import Test.QuickCheck (Arbitrary (..))
import Control.Monad.Random
import Data.Ratio

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
  deriving (Eq,Ord,Data,Num,Fractional,Real,RealFrac,Generic,Hashable,ToJSON,FromJSON)

data NatPrec (p :: Nat) = NatPrec

instance KnownNat p => HasResolution (NatPrec p) where
  resolution _ = 10 ^ (1 + natVal (NatPrec @p))


instance KnownNat p => Show (RealNumber p) where
  show r = case asFixed r of
             Exact p -> dropWhileEnd (== '.') . dropWhileEnd (== '0') . show $ p
             Lossy p -> (<> "~")                                      . show $ p

instance KnownNat p => Read (RealNumber p) where
  readsPrec i = map wrap . readsPrec @(Fixed (NatPrec p)) i
    where
      wrap (RealNumber . realToFrac -> x,s') = case s' of
                                                 '~':s'' -> (x,s'')
                                                 _       -> (x,s')

instance KnownNat p => Arbitrary (RealNumber p) where
  arbitrary = fromFixed <$> arbitrary


instance Random (RealNumber p) where
  -- Generate a random number between a and b with 'maxBound `div` 2 :: Int' discrete increments.
  randomR (a,b) = runRand $ do
    v <- liftRand random
    pure $ (b-a)*abs v + a
  -- Generate a random number between -1 and +1 with 'maxBound::Int' discrete increments.
  random = runRand $ do
    v <- liftRand random
    let fromInt :: Int -> Integer; fromInt = fromIntegral
    pure $ RealNumber $ fromInt v % fromInt maxBound

--------------------------------------------------------------------------------




data AsFixed p = Exact !(Fixed p) | Lossy !(Fixed p) deriving (Show,Eq)

toFixed :: KnownNat p => RealNumber p -> Fixed (NatPrec p)
toFixed = realToFrac

fromFixed :: KnownNat p => Fixed (NatPrec p) -> RealNumber p
fromFixed = realToFrac

asFixed   :: KnownNat p => RealNumber p -> AsFixed (NatPrec p)
asFixed r = let p = toFixed r in if r == fromFixed p then Exact p else Lossy p
