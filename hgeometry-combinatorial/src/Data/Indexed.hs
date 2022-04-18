{-# LANGUAGE  ScopedTypeVariables  #-}
module Data.Indexed
  ( HasIndex(..)
  , Index
  , WithIndex(..)
  , labelWith, labelWithIndex
  ) where


import Control.Monad.State.Strict
-- import Test.QuickCheck (Arbitrary(..))

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

-- -- | the index type used to disambiguate the values
-- data SoSI = MkSoS {-# UNPACK #-}!Index -- ^ original index
--                   {-# UNPACK #-}!Int -- ^ index of the coordinate in [0..(d-1)]
--           deriving (Show,Eq,Ord)

-- -- for now I've kept the two components separtely, as to avoid blowing
-- -- up the range required for the indices. Maybe it would be faster to
-- -- just map the jth coordinate of point p_i to index i*d+j
-- -- though. That way we can map to a Point d (Symoblic (WithIndex
-- -- r)). Maybe that way we can use IntSets and so on to represent the
-- -- Bags/Symbolic type rather than the arbitrary i as we currently
-- -- have.

-- instance Arbitrary SoSI where
--   arbitrary = MkSoS <$> arbitrary <*> arbitrary

--------------------------------------------------------------------------------

-- | Label each element with its index using the given labelling
-- function. Returns the new collection as well as its size.
labelWith           :: forall t a b. Traversable t
                    => (Index -> a -> b) -> t a
                    -> (t b, Int)
labelWith withIndex = flip runState 0 . traverse lbl
  where
    lbl   :: a -> State Int b
    lbl x = do i <- get
               put $ i+1
               pure (withIndex i x)

-- | Label each element with its index. Returns the new collection as
-- well as its size.
labelWithIndex :: Traversable  t => t a -> (t (WithIndex a), Int)
labelWithIndex = labelWith WithIndex
