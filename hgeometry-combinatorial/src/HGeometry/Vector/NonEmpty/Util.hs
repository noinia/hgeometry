{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module HGeometry.Vector.NonEmpty.Util
  (
  ) where

import           Control.Lens
import qualified Data.Foldable as F
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Semigroup.Foldable
import qualified Data.Vector as Vector
import           Data.Vector.NonEmpty.Internal (NonEmptyVector(..))

--------------------------------------------------------------------------------


type instance Index   (NonEmptyVector a) = Int
type instance IxValue (NonEmptyVector a) = a

instance Ixed (NonEmptyVector a) where
  ix i f (NonEmptyVector v) = NonEmptyVector <$> ix i f v

instance Foldable1 NonEmptyVector
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
instance FoldableWithIndex Int NonEmptyVector where
  ifoldMap f (NonEmptyVector v) = ifoldMap f v
instance TraversableWithIndex Int NonEmptyVector where
  itraverse f (NonEmptyVector v) = NonEmptyVector <$> itraverse f v
