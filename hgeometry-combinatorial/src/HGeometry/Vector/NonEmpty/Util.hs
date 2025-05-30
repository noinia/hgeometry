{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# OPTIONS_GHC -Wno-orphans #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Vector.NonEmpty.Util
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Instances for non-empty vectors
--
--------------------------------------------------------------------------------
module HGeometry.Vector.NonEmpty.Util
  (
  ) where

import           Control.Lens
import           Data.Coerce (coerce)
import qualified Data.Foldable as F
import           Data.Foldable1
import           Data.Foldable1.WithIndex
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Vector as Vector
import qualified Data.Vector.NonEmpty as NonEmptyV
import           Data.Vector.NonEmpty.Internal (NonEmptyVector(..))

--------------------------------------------------------------------------------


type instance Index   (NonEmptyVector a) = Int
type instance IxValue (NonEmptyVector a) = a

instance Ixed (NonEmptyVector a) where
  ix i f (NonEmptyVector v) = NonEmptyVector <$> ix i f v
  {-# INLINE ix #-}


instance Foldable1WithIndex Int NonEmptyVector where
  ifoldMap1 f = fold1 . NonEmptyV.imap f
  {-# INLINE ifoldMap1 #-}
-- -- | ifoldMap1. This will appear in indexedtraversal as of next release
-- ifoldMap1   :: Semigroup m => (Int -> a -> m) -> NonEmptyV.NonEmptyVector a -> m



-- instance Foldable1 NonEmptyVector where
--   foldMap1 f v = let (v',x) = NV.unsnoc v
--                  in Vector.foldr (\x' a -> f x' <> a) (f x) v'
--   {-# INLINE foldMap1 #-}

instance Traversable1 NonEmptyVector where
  traverse1 f (NonEmptyVector v) =
      -- Get the length of the vector in /O(1)/ time
      let !n = F.length v
      -- Use fromListN to be more efficient in construction of resulting vector
      -- Also behaves better with compact regions, preventing runtime exceptions
      in NonEmptyVector . Vector.fromListN n . F.toList
         <$> traverse1 f (NonEmpty.fromList $ F.toList v)
         -- notice that NonEmpty.fromList is suposedly safe since the vector is NonEmpty...
  {-# INLINE traverse1 #-}

instance FunctorWithIndex Int NonEmptyVector where
  imap f (NonEmptyVector v) = NonEmptyVector $ imap f v
  {-# INLINE imap #-}
instance FoldableWithIndex Int NonEmptyVector where
  ifoldMap f (NonEmptyVector v) = ifoldMap f v
  {-# INLINE ifoldMap #-}
instance TraversableWithIndex Int NonEmptyVector where
  itraverse f (NonEmptyVector v) = NonEmptyVector <$> itraverse f v
  {-# INLINE itraverse #-}

-- this instance belongs in NonEmptyVector, but alas
instance Reversing (NonEmptyVector a) where
  reversing = NonEmptyVector . reversing . coerce
