--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Measured.Size
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--------------------------------------------------------------------------------
module HGeometry.Measured.Size
  ( Size(Size)
  , Elem(Elem)
  , Sized(Sized)
  ) where

import Control.DeepSeq
import GHC.Generics (Generic)
import HGeometry.Measured.Class

--------------------------------------------------------------------------------

-- | Measured size. Always non-negative.
newtype Size = Size Word deriving (Show,Read,Eq,Num,Integral,Enum,Real,Ord,Generic,NFData)

instance Semigroup Size where
  x <> y = x + y

instance Monoid Size where
  mempty = Size 0

--------------------------------------------------------------------------------

-- | Newtype wrapper for things for which we can measure the size
newtype Elem a = Elem { _unElem :: a }
               deriving (Show,Read,Eq,Ord,Functor,Foldable,Traversable)

instance Measured Size (Elem a) where
  measure _ = 1

--------------------------------------------------------------------------------

-- | Things that have a size
data Sized a = Sized {-# UNPACK #-} !Size a
             deriving (Show,Eq,Ord,Functor,Foldable,Traversable,Generic)
instance NFData a => NFData (Sized a)

instance Semigroup a => Semigroup (Sized a) where
  (Sized i a) <> (Sized j b) = Sized (i <> j) (a <> b)

instance Monoid a => Monoid (Sized a) where
  mempty = Sized mempty mempty

-- instance Semigroup a => Measured Size (Sized a) where
--   measure (Sized i _) = i
