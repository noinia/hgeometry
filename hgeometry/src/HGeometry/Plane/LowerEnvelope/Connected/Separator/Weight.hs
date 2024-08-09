{-# LANGUAGE  UndecidableInstances  #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Plane.LowerEnvelope.Separator.Weight
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Computes a separator for a planar graph
--
--------------------------------------------------------------------------------
module HGeometry.Plane.LowerEnvelope.Connected.Separator.Weight
  ( HasWeight(..)
  , IsWeight(..)
  , Weighted(..)
  , weightOf
  , weightOf'
  , annotate
  ) where

import Data.Semigroup
import Data.Kind (Type)
import Data.Tree

--------------------------------------------------------------------------------

-- | Types that can act as weight. So far just Int's.
class IsWeight w where
  data Weighted w :: Type -> Type
  -- | Annotate a value with a weight
  withWeight :: w -> a -> Weighted w a

  -- | get the weight of a weighted value
  getWeight :: Weighted w a -> w
  -- | get teh value
  getValue  :: Weighted w a -> a

instance IsWeight Int where
  data Weighted Int a = Weighted {-#UNPACK#-}!Int a deriving (Show,Eq,Ord,Functor)
  withWeight = Weighted
  getWeight (Weighted w _) = w
  getValue  (Weighted _ x) = x

instance (Semigroup a, IsWeight w, Num w) => Semigroup (Weighted w a) where
  wx <> wy = withWeight (getWeight wx + getWeight wy) (getValue wx <> getValue wy)

instance (IsWeight w, Num w, Monoid a) => Monoid (Weighted w a) where
  mempty = withWeight 0 mempty



-- instance IsWeight w => IsWeight (Sum w) where
--   newtype Weighted (Sum w) a = W (Weighted w a)
--   withWeight (Sum w) = W . withWeight w
--   getWeight = getWeight
--   getValue = getValue

class IsWeight w => HasWeight t w | t -> w where
  -- | Computes the weight
  measureWeight :: t -> Weighted w t
  -- | Compute the weight of a value
  weightOf :: t -> w
  weightOf = getWeight . measureWeight

instance HasWeight (Tree a) Int where
  measureWeight t = fmap (const t) . rootLabel . annotate $ t

instance (HasWeight t w, Num w) => HasWeight [t] w where
  measureWeight xs = flip withWeight xs . sum $ map weightOf xs


-- instance Ord a => Ord (Weighted Int a) where
--   -- | compare by weight
--   (Weighted w x) `compare` (Weighted w' x') = w `compare` w' <> x `compare` x'


-- -- | Get the total weight
-- weightOf :: (Num w, IsWeight w) => [Tree (Weighted w a)] -> w
-- weightOf = sum . map (getWeight . rootLabel)

-- | recompute get the total weight
weightOf' :: (Num w, IsWeight w) => [Tree (Weighted w a)] -> w
weightOf' = sum . fmap getWeight . concatMap flatten

-- | Annotate tht tree with the size of the subtrees
annotate              :: Tree k -> Tree (Weighted Int k)
annotate (Node v chs) = let chs' = map annotate chs
                        in Node (Weighted (1 + weightOf chs') v) chs'
