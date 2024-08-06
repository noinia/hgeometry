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
  ( IsWeight(..)
  , Weighted(..)
  , weightOf
  , weightOf'
  , annotate
  ) where

import Data.Kind (Type)
import Data.Tree
import HGeometry.Plane.LowerEnvelope.Connected.Separator.Util (root)

--------------------------------------------------------------------------------

-- | Types that can act as weight. So far just Int's.
class IsWeight w where
  data Weighted w :: Type -> Type
  getWeight :: Weighted w a -> w
  getValue  :: Weighted w a -> a

instance IsWeight Int where
  data Weighted Int a = Weighted {-#UNPACK#-}!Int a deriving (Show,Eq,Ord,Functor)
  getWeight (Weighted w _) = w
  getValue  (Weighted _ x) = x

-- instance Ord a => Ord (Weighted Int a) where
--   -- | compare by weight
--   (Weighted w x) `compare` (Weighted w' x') = w `compare` w' <> x `compare` x'


-- | Get the total weight
weightOf :: (Num w, IsWeight w) => [Tree (Weighted w a)] -> w
weightOf = sum . map (getWeight . root)

-- | recompute get the total weight
weightOf' :: (Num w, IsWeight w) => [Tree (Weighted w a)] -> w
weightOf' = sum . fmap getWeight . concatMap flatten

-- | Annotate tht tree with the size of the subtrees
annotate              :: Tree k -> Tree (Weighted Int k)
annotate (Node v chs) = let chs' = map annotate chs
                        in Node (Weighted (1 + weightOf chs') v) chs'
