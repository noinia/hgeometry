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


  , insert, insertReplace
  , assocs
  , (!?), (!)
  ) where

import Control.Lens
import Data.List.NonEmpty (NonEmpty(..))
import Data.Foldable1
import Data.Map.NonEmpty (NEMap)
import Data.Map.NonEmpty qualified as NEMap
import Data.Coerce

--------------------------------------------------------------------------------

-- | A NonEmpty Map in which we combine conflicting elements by using their semigroup
-- operation rather than picking the left value (as is done in the default Data.Map)
newtype MonoidalNEMap k v = MonoidalNEMap { getNEMap :: NEMap k v }
  deriving stock (Show)
  deriving newtype (Functor,Foldable,Foldable1)

instance (Ord k, Semigroup v) => Semigroup (MonoidalNEMap k v) where
  (MonoidalNEMap ma) <> (MonoidalNEMap mb) = MonoidalNEMap $ NEMap.unionWith (<>) ma mb

-- type instance Index   (MonoidalNEMap k v) = k
-- type instance IxValue (MonoidalNEMap k v) = v

-- instance Ord k => Ixed (MonoidalNEMap k v)

-- instance Ord k => At (MonoidalNEMap k v) where
--   at k f = NEMap.alterF f k
--   {-# INLINE at #-}

instance Traversable (MonoidalNEMap k) where
  traverse f = fmap MonoidalNEMap . traverse f . getNEMap

instance Traversable1 (MonoidalNEMap k) where
  traverse1 f = fmap MonoidalNEMap . traverse1 f . getNEMap

instance FunctorWithIndex k (MonoidalNEMap k) where
  imap f = MonoidalNEMap . NEMap.mapWithKey f . getNEMap

instance FoldableWithIndex k (MonoidalNEMap k) where
  ifoldMap f = NEMap.foldMapWithKey f . getNEMap

instance TraversableWithIndex k (MonoidalNEMap k) where
  itraverse f = fmap MonoidalNEMap . NEMap.traverseWithKey f . getNEMap


--------------------------------------------------------------------------------

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


--------------------------------------------------------------------------------

-- | Return all key/value pairs in the map in ascending key order.
--
-- \(O(n)\)
assocs :: MonoidalNEMap k v -> NonEmpty (k, v)
assocs = NEMap.assocs . getNEMap

-- | Insert an new key,value pair into the Map
--
-- \(O(\log n)\)
insert     :: (Ord k, Semigroup v) => k -> v -> MonoidalNEMap k v -> MonoidalNEMap k v
insert k v = coerce $ NEMap.insertWith (<>) k v

-- | Insert an new key,value pair into the Map
-- \(O(\log n)\)
insertReplace     :: Ord k => k -> v -> MonoidalNEMap k v -> MonoidalNEMap k v
insertReplace k v = coerce $ NEMap.insert k v


infixl 9 !?
infixl 9 !

-- | Lookup in the Map
-- \(O(\log n)\)
(!?)   :: Ord k => MonoidalNEMap k v -> k -> Maybe v
(!?) m = (NEMap.!?) (coerce m)

-- | unsafe lookup in the Map
-- \(O(\log n)\)
(!)   :: Ord k => MonoidalNEMap k v -> k -> v
(!) m = (NEMap.!) (coerce m)
