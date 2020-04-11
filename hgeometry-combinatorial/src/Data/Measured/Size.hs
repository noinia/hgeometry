module Data.Measured.Size where

import Control.DeepSeq
import Data.Measured.Class
import GHC.Generics (Generic)

--------------------------------------------------------------------------------

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
