{-# LANGUAGE  ScopedTypeVariables  #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Indexed
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Things that have an index.
--
--------------------------------------------------------------------------------
module Data.Indexed
  ( HasIndex(..)
  , Index
  , WithIndex(..), theValue
  , labelWithIndex, labelWith, labelWith'
  ) where


import Control.Lens(Lens, Field1, lens, _1)
import Control.Monad.State.Strict

--------------------------------------------------------------------------------

type Index = Int

class HasIndex a where
  -- | Types that have an index.
  sosIndex :: a -> Index


data WithIndex a = WithIndex {-# UNPACK #-} !Index a
               deriving (Show)

-- | Lens to manipulate the value of the 'WithIndex'
theValue :: Lens (WithIndex a) (WithIndex b) a b
theValue = lens (\(WithIndex _ x) -> x) (\(WithIndex i _) y -> WithIndex i y)

instance Field1 (WithIndex a) (WithIndex b) a b where
  _1 = theValue

instance HasIndex (WithIndex a) where
  sosIndex (WithIndex i _) = i
  {-# INLINE sosIndex #-}

-- instance Eq a => Eq (WithIndex a) where
--   (WithIndex i x) == (WithIndex j y) = x == y && i == j

-- instance Ord a => Ord (WithIndex a) where
--   (WithIndex i x) `compare` (WithIndex j y) = x `compare` y <> Down i `compare` Down j

--------------------------------------------------------------------------------


--------------------------------------------------------------------------------

-- | Label each element with its index.
labelWithIndex :: Traversable  t => t a -> t (WithIndex a)
labelWithIndex = labelWith WithIndex

-- | Label each element with its index using the given labelling
-- function.
labelWith   :: Traversable t => (Index -> a -> b) -> t a -> t b
labelWith f = fst . labelWith' f

-- | Label each element with its index using the given labelling
-- function. Returns the new collection as well as its size.
labelWith'           :: forall t a b. Traversable t
                     => (Index -> a -> b) -> t a
                     -> (t b, Int)
labelWith' withIndex = flip runState 0 . traverse lbl
  where
    lbl   :: a -> State Int b
    lbl x = do i <- get
               put $ i+1
               pure (withIndex i x)
