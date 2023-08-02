--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Measured.Size
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--------------------------------------------------------------------------------
module HGeometry.Measured.Size
  ( Count(Count)
  , Sized(Sized)
  ) where

import Control.DeepSeq
import Data.Coerce
import Data.Monoid (Sum(..))
import GHC.Generics (Generic)
import HGeometry.Measured.Class

--------------------------------------------------------------------------------

-- | Measured size. Always non-negative.
newtype Count a = Count Word
  deriving stock (Show,Read,Eq,Ord,Generic)
  deriving newtype (NFData)
  deriving (Semigroup,Monoid) via Sum Word

instance Measured Count a where
  measure = const $ Count 1

instance CanInsert Count a where
  insertMeasure _ (Count n) = Count (n+1)

instance CanDelete Count a where
  deleteMeasure _ (Count n) = Just . Count $ max 0 (n-1)

--------------------------------------------------------------------------------

-- -- | Newtype wrapper for things for which we can measure the size
-- newtype Elem a = Elem { _unElem :: a }
--                deriving (Show,Read,Eq,Ord,Functor,Foldable,Traversable)

-- instance Measured Size (Elem a) where
--   measure _ = 1

--------------------------------------------------------------------------------

-- | Things that have a size
data Sized a = Sized {-# UNPACK #-} !(Count a) a
             deriving (Show,Eq,Ord,Generic)
instance NFData a => NFData (Sized a)

instance Functor Sized where
  fmap f (Sized c a) = Sized (coerce c) (f a)
instance Foldable Sized where
  foldMap f (Sized _ a) = f a
instance Traversable Sized where
  traverse f (Sized c a) = Sized (coerce c) <$> f a

instance Semigroup a => Semigroup (Sized a) where
  (Sized i a) <> (Sized j b) = Sized (i <> j) (a <> b)

instance Monoid a => Monoid (Sized a) where
  mempty = Sized mempty mempty

-- instance Semigroup a => Measured Size (Sized a) where
--   measure (Sized i _) = i
