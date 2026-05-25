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
  , labelWithIndex
  ) where


import Control.Lens
import Data.Foldable1
import Control.DeepSeq
import GHC.Generics(Generic)
import Control.Monad.State.Strict

--------------------------------------------------------------------------------

-- | Helper data type for which the Ord and Eq instance are by index.
--
-- Note that one may use the index as some sort of optimization.
-- i.e. if the indices are the same, then the values are considerd the
-- same
data ByIndex ix a = ByIndex { _theIndex :: !ix
                            , _theValue :: !a
                            }
  deriving stock (Functor,Foldable,Traversable,Generic,Show)

makeLenses ''ByIndex

instance (NFData i, NFData a) => NFData (ByIndex i a)

instance Eq ix => Eq (ByIndex ix a) where
  x == y = _theIndex x == _theIndex y

instance Ord ix => Ord (ByIndex ix a) where
  x `compare` y = _theIndex x `compare` _theIndex y

instance Foldable1 (ByIndex ix) where
  foldMap1 f (ByIndex _ x) = f x

instance Traversable1 (ByIndex ix) where
  traverse1 f (ByIndex i x) = ByIndex i <$> f x


-- | Label each element with its index.
labelWithIndex :: (Traversable t) => t a -> t (ByIndex Int a)
labelWithIndex = labelWith ByIndex

-- | Label each element with its index using the given labelling
-- function.
labelWith   :: Traversable t => (Int -> a -> b) -> t a -> t b
labelWith f = fst . labelWith' f

-- | Label each element with its index using the given labelling
-- function. Returns the new collection as well as its size.
labelWith'           :: forall t a b. Traversable t
                     => (Int -> a -> b) -> t a
                     -> (t b, Int)
labelWith' withIndex' = flip runState 0 . traverse lbl
  where
    lbl   :: a -> State Int b
    lbl x = do i <- get
               put $ i+1
               pure (withIndex' i x)
