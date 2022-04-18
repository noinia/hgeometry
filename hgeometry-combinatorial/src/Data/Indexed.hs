module Data.Indexed
  ( HasIndex(..)
  , Index
  , WithIndex(..)
  ) where

--------------------------------------------------------------------------------

type Index = Int

class HasIndex a where
  -- | Types that have an index.
  sosIndex :: a -> Index


data WithIndex a = WithIndex {-# UNPACK #-} !Index a
               deriving (Show)


instance HasIndex (WithIndex a) where
  sosIndex (WithIndex i _) = i
  {-# INLINE sosIndex #-}

-- instance Eq a => Eq (WithIndex a) where
--   (WithIndex i x) == (WithIndex j y) = x == y && i == j

-- instance Ord a => Ord (WithIndex a) where
--   (WithIndex i x) `compare` (WithIndex j y) = x `compare` y <> Down i `compare` Down j


--------------------------------------------------------------------------------
