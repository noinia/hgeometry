{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.ByIndex
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Types that have an Index, which is used to distinguish the
-- items. Hence, Eq and Ord are *only* based on the Index.
--
--------------------------------------------------------------------------------
module HGeometry.ByIndex
  ( ByIndex(ByIndex), theIndex, theValue
  ) where


import Control.Lens
import Data.Foldable1

--------------------------------------------------------------------------------

-- | Helper data type for which the Ord and Eq instance are by index.
--
-- Note that one may use the index as some sort of optimization.
-- i.e. if the indices are the same, then the values are considerd the
-- same
data ByIndex ix a = ByIndex { _theIndex :: !ix
                            , _theValue :: !a
                            }
  deriving stock (Functor,Foldable,Traversable)

makeLenses ''ByIndex

instance Eq ix => Eq (ByIndex ix a) where
  x == y = _theIndex x == _theIndex y

instance Ord ix => Ord (ByIndex ix a) where
  x `compare` y = _theIndex x `compare` _theIndex y

instance Foldable1 (ByIndex ix) where
  foldMap1 f (ByIndex _ x) = f x

instance Traversable1 (ByIndex ix) where
  traverse1 f (ByIndex i x) = ByIndex i <$> f x
