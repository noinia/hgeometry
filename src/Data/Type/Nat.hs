{-# LANGUAGE UndecidableInstances #-}    -- For the Nat1 to Nat conversions
module Data.Type.Nat where

import GHC.TypeLits

--------------------------------------------------------------------------------

-- | Unary implementation of natural numbers.
-- Used both at the type and at the value level.
data Nat1 = Zero | Succ Nat1

type family FromNat1 (n :: Nat1) :: Nat where
  FromNat1 Zero     = 0
  FromNat1 (Succ n) = 1 + FromNat1 n

type family ToNat1 (n :: Nat) :: Nat1 where
  ToNat1 0 = Zero
  ToNat1 n = Succ (ToNat1 (n - 1))
