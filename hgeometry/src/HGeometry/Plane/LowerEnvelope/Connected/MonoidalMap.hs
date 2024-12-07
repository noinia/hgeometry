module HGeometry.Plane.LowerEnvelope.Connected.MonoidalMap
  ( MonoidalMap(..)
  , unionsWithKey
  , mapWithKeyMerge

  , MonoidalNEMap(..)
  , mapWithKeyMerge1
  ) where

import qualified Data.Foldable as F
import           Data.Foldable1
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Map.NonEmpty (NEMap)
import qualified Data.Map.NonEmpty as NEMap

--------------------------------------------------------------------------------
-- * Operations on Maps

-- | Merge a bunch of maps
unionsWithKey   :: (Foldable f, Ord k) => (k -> a-> a ->a) -> f (Map k a) -> Map k a
unionsWithKey f = F.foldl' (Map.unionWithKey f) Map.empty


-- | Merge the maps. When they share a key, combine their values using a semigroup.
mapWithKeyMerge   :: (Ord k', Semigroup v')
                  => (k -> v -> Map k' v') -> Map k v -> Map k' v'
mapWithKeyMerge f = getMap . Map.foldMapWithKey (\k v -> MonoidalMap $ f k v)


-- | A Map in which we combine conflicting elements by using their semigroup operation
-- rather than picking the left value (as is done in the default Data.Map)
newtype MonoidalMap k v = MonoidalMap { getMap :: Map k v }
  deriving stock (Show)
  deriving newtype (Functor,Foldable)

instance (Ord k, Semigroup v) => Semigroup (MonoidalMap k v) where
  (MonoidalMap ma) <> (MonoidalMap mb) = MonoidalMap $ Map.unionWith (<>) ma mb

instance (Ord k, Semigroup v) => Monoid (MonoidalMap k v) where
  mempty = MonoidalMap mempty

--------------------------------------------------------------------------------

-- -- | Merge a bunch of maps
-- unions1WithKey   :: (Foldable1 f, Ord k) => (k -> a-> a ->a) -> f (NEMap k a) -> NEMap k a
-- unions1WithKey f = F.foldl' (NEMap.unionWithKey f) Map.empty


-- | Merge the maps. When they share a key, combine their values using a semigroup.
mapWithKeyMerge1   :: (Ord k', Semigroup v')
                   => (k -> v -> NEMap k' v') -> NEMap k v -> NEMap k' v'
mapWithKeyMerge1 f = getNEMap . NEMap.foldMapWithKey (\k v -> MonoidalNEMap $ f k v)

-- | A NonEmpty Map in which we combine conflicting elements by using their semigroup
-- operation rather than picking the left value (as is done in the default Data.Map)
newtype MonoidalNEMap k v = MonoidalNEMap { getNEMap :: NEMap k v }
  deriving stock (Show)
  deriving newtype (Functor,Foldable,Foldable1)

instance (Ord k, Semigroup v) => Semigroup (MonoidalNEMap k v) where
  (MonoidalNEMap ma) <> (MonoidalNEMap mb) = MonoidalNEMap $ NEMap.unionWith (<>) ma mb
