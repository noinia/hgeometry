--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Small.AtMostTwo
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- A type expressing that some value may have two or three values
--
--------------------------------------------------------------------------------
module HGeometry.Small.TwoOrThree
  ( TwoOrThree(..)
  ) where

import Data.Foldable1
import Data.Bifunctor
import Data.Bitraversable
import Data.Semigroup.Bitraversable
import Data.Semigroup.Bifoldable
import Data.Semigroup.Traversable
import Linear.V2(V2(..))
import Linear.V3(V3(..))

--------------------------------------------------------------------------------


-- | Either two or three elements
newtype TwoOrThree a = TwoOrThree (Either (V2 a) (V3 a))
                     deriving (Show,Eq,Ord)

instance Functor TwoOrThree where
  fmap f (TwoOrThree e) = TwoOrThree (bimap (fmap f) (fmap f) e)
instance Foldable1 TwoOrThree where
  foldMap1 f (TwoOrThree e) = bifoldMap1 (foldMap1 f) (foldMap1 f) e
instance Foldable TwoOrThree where
  foldMap = foldMap1
instance Traversable1 TwoOrThree where
  traverse1 f (TwoOrThree e) = TwoOrThree <$> bitraverse1 (traverse1 f) (traverse1 f) e
instance Traversable TwoOrThree where
  traverse f (TwoOrThree e) = TwoOrThree <$> bitraverse (traverse f) (traverse f) e
