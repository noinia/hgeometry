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
  , assocs
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

-- | Get the data as an 'f' of (k,v) pairs.
assocs          :: Foldable f => KV f k v -> f (k,v)
assocs (KV xs) = xs


deriving instance (Show k, Show v, Show1 f) => Show (KV f k v)
deriving instance (Eq k, Eq v, Eq1 f)       => Eq (KV f k v)
deriving instance (Ord k, Ord v, Ord1 f)    => Ord (KV f k v)

deriving instance Functor f     => Functor (KV f k)
deriving instance Foldable f    => Foldable (KV f k)
deriving instance Traversable f => Traversable (KV f k)

-- instance (Show1 f, Show k) => Show1 (KV f k)
-- instance Show1 f           => Show2 (KV f) where
--   liftShowsPrec2

instance (Eq1 f, Eq k) => Eq1 (KV f k)
instance Eq1 f         => Eq2 (KV f) where
  liftEq2 eqK eqV (KV xs) (KV ys) = liftEq (\(k,v) (k',v') -> eqK k k' && eqV v v') xs ys

instance (Ord1 f, Ord k) => Ord1 (KV f k)
instance Ord1 f         => Ord2 (KV f) where
  liftCompare2 cmpK cmpV (KV xs) (KV ys) =
    liftCompare (\(k,v) (k',v') -> cmpK k k' <> cmpV v v') xs ys


instance (Show1 f, Show k) => Show1 (KV f k)
instance Show1 f => Show2 (KV f) where
  liftShowsPrec2 sp1 sl1 sp2 sl2 d (KV xs) = showsUnaryWith (liftShowsPrec sp sl) "KV" d xs
    where
      sp = liftShowsPrec2 sp1 sl1 sp2 sl2
      sl = undefined


--   liftShowsPrec2 sp1 _ sp2 _ _ (x, y) =
--         showChar '(' . sp1 0 x . showChar ',' . sp2 0 y . showChar ')'

-- instance Eq1 f   => Eq2   (KV f)
-- instance Ord1 f  => Ord2  (KV f)


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
