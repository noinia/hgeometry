--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Foldable.Util
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Convenience classes for types that can be built from foldable collections.
--
--------------------------------------------------------------------------------
module HGeometry.Foldable.Util
  ( HasFromFoldable(..)
  , HasFromFoldable1(..)
  ) where

import qualified Data.Foldable as F
import           Data.Foldable1
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Sequence as Seq
import qualified Data.Vector as Vector
import qualified Data.Vector.NonEmpty as NonEmptyVector
import           Data.Vector.NonEmpty.Internal (NonEmptyVector(..))

--------------------------------------------------------------------------------

-- | Types that can be built from foldable collections
class HasFromFoldable f where
  -- | Build the data structure from something that is foldable.
  fromFoldable :: Foldable g => g a -> f a
  fromFoldable = fromList . F.toList

  -- | Build the data structure from a list of elements
  fromList :: [a] -> f a
  {-# MINIMAL fromList #-}

-- | Types that can be built from non-empty foldable collections
class HasFromFoldable1 f where
  -- | Build the data structure from something that is foldable and
  -- has at least one element.
  fromFoldable1 :: Foldable1 g => g a -> f a
  fromFoldable1 = fromNonEmpty . toNonEmpty

  -- | Build the data structure from a non-empty list of elements
  fromNonEmpty :: NonEmpty a -> f a
  {-# MINIMAL fromNonEmpty #-}

instance HasFromFoldable1 NonEmpty where
  fromNonEmpty = id

instance HasFromFoldable Vector.Vector  where
  fromList = Vector.fromList


instance HasFromFoldable1 NonEmptyVector  where
  fromNonEmpty = NonEmptyVector.fromNonEmpty

instance HasFromFoldable Seq.Seq where
  fromList = Seq.fromList
