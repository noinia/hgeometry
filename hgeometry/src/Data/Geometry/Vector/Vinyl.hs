{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Geometry.Vector.Vinyl where

import           Data.Traversable


import           Control.Applicative hiding (Const(..))
import           Data.Foldable
import           Data.Traversable
import           Data.Typeable

import qualified Data.Vinyl as V
import           Data.Vinyl hiding (Nat)
import qualified Data.Vinyl.TypeLevel as TV

import           Data.Vinyl.Functor


import           Data.Singletons
import           Data.Singletons.TH
import           Data.Singletons.Prelude.Bool(If, (:&&))
import           Data.Singletons.Prelude.Base(IdSym0)
import           GHC.TypeLits

import qualified Linear.V3 as L3
import           Linear.Affine
import           Linear.Metric
import           Linear.Vector


--------------------------------------------------------------------------------

type DimRange (d :: Nat) = Range 1 d

type Range s k = Range1 IdSym0 s (ToVNat k)

type family Range1 (f :: TyFun Nat t -> *) (s :: Nat) (k :: TV.Nat) :: [t] where
  Range1 f s TV.Z     = '[]
  Range1 f s (TV.S k) = Apply f s ': Range1 f (1 + s) k

-- | Check if a given number n is in the Dimrange of d
type family (n :: Nat) `InDimRange` (d :: Nat) :: Bool where
  n `InDimRange` d= (1 <=? n) :&& (n <=? d)

--------------------------------------------------------------------------------

-- Converting between GHC.TypeLits.Nat and Vinyls Nats

type family FromVNat (n :: TV.Nat) where
  FromVNat TV.Z     = 0
  FromVNat (TV.S n) = 1 + FromVNat n

type family ToVNat (n :: Nat) where
  ToVNat 0 = TV.Z
  ToVNat n = TV.S (ToVNat (n - 1))

--------------------------------------------------------------------------------

-- | Proxy type to index the coordinate fields of a vector and/or point
data C (n :: Nat) where C :: C n


--------------------------------------------------------------------------------

newtype Vector d r =
  Vector { unVec :: Rec (Const r) (DimRange d) }

deriving instance Eq (Rec (Const r) (DimRange d))  => Eq (Vector d r)
deriving instance Ord (Rec (Const r) (DimRange d)) => Ord (Vector d r)

instance Functor (Vector d) where
  fmap = fmapDefault
instance Foldable (Vector d) where
  foldMap = foldMapDefault

instance Traversable (Vector d) where
  traverse f (Vector r) = Vector <$> rtraverse (lift f) r
    where
      lift :: Applicative f => (a -> f b) -> Const a k -> f (Const b k)
      lift f (Const a) = Const <$> f a

-- | This constraint is always satisfied
instance RecApplicative (DimRange d) => Applicative (Vector d) where
  pure x = Vector $ rpure (Const x)
  (Vector fs) <*> (Vector rs) = Vector $ fs' `rapply` rs
    where
      -- fs'  :: Rec (Lift (->) (Const a) (Const b)) rs
      fs' = rmap (Lift . fmap' . getConst) fs
      -- fmap in the first argument of Const
      fmap' :: (a -> b) -> (Const a k -> Const b k)
      fmap' f (Const a) = Const $ f a

instance RecApplicative (DimRange d) => Additive (Vector d) where
  zero = pure 0
  u ^+^ v = (+) <$> u <*> v

instance RecApplicative (DimRange d) => Affine (Vector d) where
  type Diff (Vector d) = Vector d
  u .-. v = u ^-^ v
  p .+^ v = p ^+^ v

instance RecApplicative (DimRange d) => Metric (Vector d)

cross       :: Num r => Vector 3 r -> Vector 3 r -> Vector 3 r
u `cross` v = fromV3 $ (toV3 u) `L3.cross` (toV3 v)
  where
    toV3 (Vector v) = let a = getConst . rget (C :: C 1) $ v
                          b = getConst . rget (C :: C 2) $ v
                          c = getConst . rget (C :: C 3) $ v
                      in L3.V3 a b c
    fromV3 (L3.V3 a b c) = Vector $ Const a :& Const b :& Const c :& RNil
