module HGeometry.Plane.LowerEnvelope.Connected.MonoidalMap
  ( MonoidalMap, getMap
  , unionsWithKey
  , mapWithKeyMerge
  ) where

import qualified Data.Foldable as F
import           Data.Map (Map)
import qualified Data.Map as Map

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
  deriving (Show)

instance (Ord k, Semigroup v) => Semigroup (MonoidalMap k v) where
  (MonoidalMap ma) <> (MonoidalMap mb) = MonoidalMap $ Map.unionWith (<>) ma mb

instance (Ord k, Semigroup v) => Monoid (MonoidalMap k v) where
  mempty = MonoidalMap mempty
