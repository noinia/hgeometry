--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Map.NonEmpty.Monoidal
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- A NonEmpty Monoidal Map, i.e. combining the monoidal maps combines the elements with
-- their semigroup operation.
--
--------------------------------------------------------------------------------
module HGeometry.Map.NonEmpty.Monoidal
  ( MonoidalNEMap(..)
  , singleton
  , unions1WithKey
  , mapWithKeyMerge1
  ) where

import           Data.Foldable1
import           Data.Map.NonEmpty (NEMap)
import qualified Data.Map.NonEmpty as NEMap

--------------------------------------------------------------------------------

-- | A NonEmpty Map in which we combine conflicting elements by using their semigroup
-- operation rather than picking the left value (as is done in the default Data.Map)
newtype MonoidalNEMap k v = MonoidalNEMap { getNEMap :: NEMap k v }
  deriving stock (Show)
  deriving newtype (Functor,Foldable,Foldable1)

instance (Ord k, Semigroup v) => Semigroup (MonoidalNEMap k v) where
  (MonoidalNEMap ma) <> (MonoidalNEMap mb) = MonoidalNEMap $ NEMap.unionWith (<>) ma mb

-- | Create a singleton MonoidalNE Map
singleton     :: k -> v -> MonoidalNEMap k v
singleton k v = MonoidalNEMap $ NEMap.singleton k v

-- | Merge a bunch of non-empty maps with the given mergin function
unions1WithKey   :: (Foldable1 f, Ord k) => (k -> a-> a ->a) -> f (NEMap k a) -> NEMap k a
unions1WithKey f = foldl1' (NEMap.unionWithKey f)

-- | Merge the maps. When they share a key, combine their values using a semigroup.
mapWithKeyMerge1   :: (Ord k', Semigroup v')
                   => (k -> v -> NEMap k' v') -> NEMap k v -> NEMap k' v'
mapWithKeyMerge1 f = getNEMap . NEMap.foldMapWithKey (\k v -> MonoidalNEMap $ f k v)
