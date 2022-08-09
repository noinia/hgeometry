module Data.Foldable.Util
  ( HasFromFoldable(..)
  ,  HasFromFoldable1(..)
  ) where

import qualified Data.Foldable as F
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Semigroup.Foldable
import qualified Data.Vector as Vector
import qualified Data.Vector.NonEmpty as NonEmptyVector
import           Data.Vector.NonEmpty.Internal (NonEmptyVector(..))


--------------------------------------------------------------------------------

class HasFromFoldable f where
  fromFoldable :: Foldable g => g a -> f a
  fromFoldable = fromList . F.toList

  fromList :: [a] -> f a
  {-# MINIMAL fromList #-}

-- instance HasFromFoldable1 [] where
--   fromList = id

class HasFromFoldable1 f where
  fromFoldable1 :: Foldable1 g => g a -> f a
  fromFoldable1 = fromNonEmpty . toNonEmpty

  fromNonEmpty :: NonEmpty a -> f a
  {-# MINIMAL fromNonEmpty #-}

instance HasFromFoldable1 NonEmpty where
  fromNonEmpty = id

instance HasFromFoldable Vector.Vector  where
  fromList = Vector.fromList

instance HasFromFoldable1 NonEmptyVector  where
  fromNonEmpty = NonEmptyVector.fromNonEmpty
