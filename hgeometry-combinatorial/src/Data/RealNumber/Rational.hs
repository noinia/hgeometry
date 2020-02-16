{-# LANGUAGE DeriveDataTypeable #-}
module Data.RealNumber.Rational where

import Data.Data
import Data.Fixed
import Data.List (dropWhileEnd)
import GHC.Generics (Generic(..))

newtype R = RealNumber Rational
  deriving (Eq,Ord,Data,Num,Fractional,Real,RealFrac,Generic)

instance Show R where
  showsPrec i = fmap (dropWhileEnd (== '0')) . showsPrec i . realToFrac @R @Pico

instance Read R where
  readsPrec i = \s -> map (\(x,s') -> (RealNumber . realToFrac @Pico $ x,s')) $ readsPrec i s
