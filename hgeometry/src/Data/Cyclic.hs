{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE QuantifiedConstraints #-}
module Data.Cyclic
  ( Cyclic(..)
  , toCircularVector
  ) where

--------------------------------------------------------------------------------

import           Control.Lens
import qualified Data.Foldable as F
import           Data.Foldable.Util
import           Data.Semigroup.Foldable
import           Data.Vector.Circular (CircularVector(..))
import           Data.Vector.NonEmpty (NonEmptyVector)
--------------------------------------------------------------------------------

-- | A cyclic sequence type
newtype Cyclic v a = Cyclic (v a)
 deriving newtype (Functor,Foldable)

instance Foldable1 v    => Foldable1    (Cyclic v)

instance Traversable1 v => Traversable1 (Cyclic v) where
  traverse1 f (Cyclic v) = Cyclic <$> traverse1 f v
instance Traversable v => Traversable (Cyclic v) where
  traverse f (Cyclic v) = Cyclic <$> traverse f v


instance FunctorWithIndex i v => FunctorWithIndex i (Cyclic v) where
  imap f (Cyclic v) = Cyclic $ imap f v
instance FoldableWithIndex i v => FoldableWithIndex i (Cyclic v) where
  ifoldMap f (Cyclic v) = ifoldMap f v
instance TraversableWithIndex i v => TraversableWithIndex i (Cyclic v) where
  itraverse f (Cyclic v) = Cyclic <$> itraverse f v

instance HasFromFoldable v => HasFromFoldable (Cyclic v)  where
  fromFoldable = Cyclic . fromFoldable
  fromList = Cyclic . fromList

instance HasFromFoldable1 v => HasFromFoldable1 (Cyclic v)  where
  fromFoldable1 = Cyclic . fromFoldable1
  fromNonEmpty  = Cyclic . fromNonEmpty

type instance Index   (Cyclic v a) = Index   (v a)
type instance IxValue (Cyclic v a) = IxValue (v a)

instance (Index (v a) ~ Int, Foldable v, Ixed (v a)) => Ixed (Cyclic v a) where
  ix i = \f (Cyclic v) -> let n = F.length v
                          in Cyclic <$> ix (i `mod` n) f v

-- | Turn the cyclic vector into a circular Vector
toCircularVector            :: Cyclic NonEmptyVector a -> CircularVector a
toCircularVector (Cyclic v) = CircularVector v 0
