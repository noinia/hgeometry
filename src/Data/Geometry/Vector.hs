{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Geometry.Vector where

import Control.Applicative

import Data.Foldable
import Data.Traversable


import Data.Vector.Fixed.Boxed
import Data.Vector.Fixed.Cont(Z(..),S(..),ToPeano(..))

import Data.Type.Nat
import GHC.TypeLits

import Linear.Affine
import Linear.Vector

import qualified Data.Vector.Fixed as V

--------------------------------------------------------------------------------

-- | Wrapper around Vec that converts from their Peano numbers to Our peano numbers
data Vector (d :: Nat1) (r :: *) where
  Vector :: Vec (Nat1ToPeano d) r -> Vector d r

unV            :: (p ~ Nat1ToPeano d) => Vector d r -> Vec p r
unV (Vector v) = v

----------------------------------------

-- All these instances are basically the standard thing deriving would produce


type Arity  (n :: Nat)  = V.Arity (ToPeano n)
type Arity1 (n :: Nat1) = V.Arity (Nat1ToPeano n)


instance (Arity1 d, Eq a) => Eq (Vector d a) where
  (Vector u) == (Vector v) = u == v

instance (Arity1 d, Ord a) => Ord (Vector d a) where
  (Vector u) <= (Vector v) = u <= v

instance (Arity1 d, Show a) => Show (Vector d a) where
  show (Vector v) = "Vector " ++ show v

instance Arity1 d => Functor (Vector d) where
  fmap f (Vector v) = Vector $ fmap f v

instance Arity1 d => Applicative (Vector d) where
  pure                        = Vector . pure
  (Vector fv)  <*> (Vector v) = Vector $ fv <*> v

instance Arity1 d => Foldable (Vector d) where
  foldMap f (Vector v) = foldMap f v

instance Arity1 d => Traversable (Vector d) where
  traverse f (Vector v) = Vector <$> traverse f v





instance Arity1 d => Additive (Vector d) where
  zero = pure 0
  (Vector u) ^+^ (Vector v) = Vector $ V.zipWith (+) u v

instance Arity1 d => Affine (Vector d) where
  type Diff (Vector d) = Vector d

  u .-. v = u ^-^ v
  p .+^ v = p ^+^ v


type instance V.Dim (Vector d) = Nat1ToPeano d

instance Arity1 d => V.Vector (Vector d) r where
  construct    = Vector <$> V.construct
  inspect    v = V.inspect (unV v)
  basicIndex v = V.basicIndex (unV v)






----------------------------------------

-- | Get the head and tail of a vector
destruct            :: Arity1 d
                    => Vector (Succ d) r -> (r, Vector d r)
destruct (Vector v) = (V.head v, Vector $ V.tail v)
