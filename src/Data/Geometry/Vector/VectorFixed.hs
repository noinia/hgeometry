{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Geometry.Vector where

import           Control.Applicative

import           Data.Foldable
import           Data.Traversable

import           Data.Vector.Fixed.Boxed
import           Data.Vector.Fixed.Cont(Z(..),S(..),ToPeano(..),ToNat(..))

import           GHC.TypeLits

import           Linear.Affine
import           Linear.Metric
import           Linear.Vector

import qualified Data.Vector.Fixed as V

import qualified Linear.V3 as L3

--------------------------------------------------------------------------------

data Vector (d :: Nat) (r :: *) where
  Vector :: Vec (ToPeano d) r -> Vector d r

unV            :: (p ~ ToPeano d) => Vector d r -> Vec p r
unV (Vector v) = v

----------------------------------------

-- All these instances are basically the standard thing deriving would produce


type Arity  (n :: Nat)  = V.Arity (ToPeano n)
-- type Arity1 (n :: Nat1) = V.Arity (Nat1ToPeano n)


instance (Arity d, Eq a) => Eq (Vector d a) where
  (Vector u) == (Vector v) = u == v

instance (Arity d, Ord a) => Ord (Vector d a) where
  (Vector u) <= (Vector v) = u <= v

instance (Arity d, Show a) => Show (Vector d a) where
  show (Vector v) = "Vector " ++ show v

instance Arity d => Functor (Vector d) where
  fmap f (Vector v) = Vector $ fmap f v

instance Arity d => Applicative (Vector d) where
  pure                        = Vector . pure
  (Vector fv)  <*> (Vector v) = Vector $ fv <*> v

instance Arity d => Foldable (Vector d) where
  foldMap f (Vector v) = foldMap f v

instance Arity d => Traversable (Vector d) where
  traverse f (Vector v) = Vector <$> traverse f v





instance Arity d => Additive (Vector d) where
  zero = pure 0
  (Vector u) ^+^ (Vector v) = Vector $ V.zipWith (+) u v

instance Arity d => Affine (Vector d) where
  type Diff (Vector d) = Vector d

  u .-. v = u ^-^ v
  p .+^ v = p ^+^ v


instance Arity d => Metric (Vector d) where
  (Vector u) `dot` (Vector v) = V.sum $ V.zipWith (*) u v


type instance V.Dim (Vector d) = ToPeano d

instance Arity d => V.Vector (Vector d) r where
  construct    = Vector <$> V.construct
  inspect    v = V.inspect (unV v)
  basicIndex v = V.basicIndex (unV v)

----------------------------------------

-- | Get the head and tail of a vector
destruct            :: (Arity dp, ToPeano d ~ S (ToPeano dp))
                    => Vector d r -> (r, Vector dp r)
destruct (Vector v) = (V.head v, Vector $ V.tail v)


cross       :: Num r => Vector 3 r -> Vector 3 r -> Vector 3 r
u `cross` v = fromV3 $ (toV3 u) `L3.cross` (toV3 v)
  where
    toV3 vv              = let [a,b,c] = V.toList vv in L3.V3 a b c
    fromV3 (L3.V3 a b c) = Vector $ V.mk3 a b c

--------------------------------------------------------------------------------
