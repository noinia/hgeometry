module Data.TypeLevel.Common where

import Data.Vinyl.TypeLevel

type family (n :: Nat) <= (m :: Nat) :: Bool where
  Z     <= m     = True
  (S n) <= Z     = False
  (S n) <= (S m) = n <= m
