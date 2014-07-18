{-# LANGUAGE UndecidableInstances #-}    -- For the Nat1 to Nat conversions
module Data.Type.Nat where

import GHC.TypeLits

import Data.Vector.Fixed.Cont(Z(..),S(..),ToPeano(..), ToNat(..))

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



-- | Nat and Nat1 form an Isomorphism
type Nat1Iso (n :: Nat) = n ~ FromNat1 (ToNat1 n)



----------------------------------------
-- | Isomorphism between our Nat1 and the Peano numbers defined in Vector.Fixed

type family PeanoToNat1 (n :: *) :: Nat1 where
  PeanoToNat1 Z = Zero
  PeanoToNat1 (S n) = Succ (PeanoToNat1 n)

type family Nat1ToPeano (n :: Nat1) :: * where
  Nat1ToPeano Zero     = Z
  Nat1ToPeano (Succ n) = S (Nat1ToPeano n)



type PeanoNat1Iso (n :: Nat) = ToPeano n ~ Nat1ToPeano (ToNat1 n)

type NatPeano (n :: Nat) = n ~ ToNat (ToPeano n)


type NatsIso (n :: Nat) = (Nat1Iso n, NatPeano n, PeanoNat1Iso n)
