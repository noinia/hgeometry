{-# LANGUAGE UndecidableInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Sequence.KV
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Sequences of key value pairs.
--
--------------------------------------------------------------------------------
module HGeometry.Sequence.KV
  ( KV(..)
  , empty
  ) where

import Control.Lens
import Data.Bifoldable
import Data.Bitraversable
import Data.Foldable1
import Data.Functor.Classes

-----------------------------------------------------------------------------------------

-- | An 'f' of key value pairs
newtype KV f k v = KV (f (k,v))

deriving instance (Show k, Show v, Show1 f) => Show (KV f k v)
deriving instance (Eq k, Eq v, Eq1 f)       => Eq (KV f k v)
deriving instance (Ord k, Ord v, Ord1 f)    => Ord (KV f k v)

deriving instance Functor f     => Functor (KV f k)
deriving instance Foldable f    => Foldable (KV f k)
deriving instance Traversable f => Traversable (KV f k)

instance Foldable1 f    => Foldable1 (KV f e) where
  foldMap1 f (KV xs) = foldMap1 (foldMap1 f) xs

instance Traversable1 f => Traversable1 (KV f e) where
  traverse1 f (KV xs) = KV <$> traverse1 (traverse1 f) xs

instance Functor f => Bifunctor (KV f) where
  bimap f g (KV xs) = KV $ fmap (bimap f g) xs

instance Foldable f => Bifoldable (KV f) where
  bifoldMap f g (KV xs) = foldMap (bifoldMap f g) xs

instance Traversable f => Bitraversable (KV f) where
  bitraverse f g (KV xs) = KV <$> traverse (bitraverse f g) xs

instance Functor f => FunctorWithIndex k (KV f k) where
  imap f (KV xs) = KV $ fmap (\(k,v) -> (k,f k v)) xs

instance Foldable f => FoldableWithIndex k (KV f k) where
  ifoldMap f (KV xs) = foldMap (uncurry f) xs

instance Traversable f => TraversableWithIndex k (KV f k) where
  itraverse f (KV xs) = KV <$> traverse (\(k,v) -> (k,) <$> f k v) xs


instance Semigroup (f (k,v)) => Semigroup (KV f k v) where
  (KV xs) <> (KV ys) = KV $ xs <> ys

instance Monoid (f (k,v)) => Monoid (KV f k v) where
  mempty = empty

-- | Produce an empty structure
empty :: Monoid (f (k,v)) => KV f k v
empty = KV mempty
