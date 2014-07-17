{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Geometry.Vector where

import Control.Applicative

import Linear.Affine
import Linear.Vector

import Data.Vector.Fixed.Boxed
import Data.Vector.Fixed(Arity)
import Data.Vector.Fixed.Cont(Z(..),S(..))

import Data.Type.Nat
import GHC.TypeLits

import qualified Data.Vector.Fixed as V

--------------------------------------------------------------------------------

-- | Wrapper around Vec that converts from their Peano numbers to Our peano numbers
data Vector (d :: Nat1) (r :: *) where
  Vector :: Vec (Nat1ToPeano d) r -> Vector d r

instance Arity (Nat1ToPeano d) => Functor (Vector d) where
  fmap f (Vector v) = Vector $ fmap f v


instance Arity (Nat1ToPeano d) => Applicative (Vector d) where
  pure                        = Vector . pure
  (Vector fv)  <*> (Vector v) = Vector $ fv <*> v

instance Arity (Nat1ToPeano d) => Additive (Vector d) where
  zero = pure 0
  (Vector u) ^+^ (Vector v) = Vector $ V.zipWith (+) u v

instance Arity (Nat1ToPeano d) => Affine (Vector d) where
  type Diff (Vector d) = Vector d

  u .-. v = u ^-^ v
  p .+^ v = p ^+^ v

----------------------------------------
-- | Isomorphism between our Nat1 and the Peano numbers defined in Vector.Fixed

type family PeanoToNat1 (n :: *) :: Nat1 where
  PeanoToNat1 Z = Zero
  PeanoToNat1 (S n) = Succ (PeanoToNat1 n)

type family Nat1ToPeano (n :: Nat1) :: * where
  Nat1ToPeano Zero     = Z
  Nat1ToPeano (Succ n) = S (Nat1ToPeano n)

----------------------------------------

-- | Get the head and tail of a vector
destruct            :: Arity (Nat1ToPeano d)
                    => Vector (Succ d) r -> (r, Vector d r)
destruct (Vector v) = (V.head v, Vector $ V.tail v)
