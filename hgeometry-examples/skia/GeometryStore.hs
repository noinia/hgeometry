{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE UndecidableInstances #-}
module GeometryStore
  ( GeometryStore, GeometryStore'
  , empty
  , Key
  , insertNew
  , insertNew'

  ) where

import           Control.Lens
import qualified Data.Dependent.Map as DMap
import           Data.Dependent.Sum
import           Data.GADT.Compare
import           Data.GADT.Show
import           Unsafe.Coerce (unsafeCoerce)
--------------------------------------------------------------------------------

newtype Key v = Key Int
  deriving (Show,Eq,Ord)


instance GEq Key where
  geq (Key i) (Key j)
    | i == j    = Just $ unsafeCoerce Refl
                  -- this is safe since we keep the Keys to ourselves
    | otherwise = Nothing

instance GCompare Key where
  gcompare (Key i) (Key j) = case i `compare` j of
                               LT -> GLT
                               EQ -> unsafeCoerce GEQ
                                 -- this is safe since we keep the Keys to ourselves
                               GT -> GGT

instance GShow Key where
  gshowsPrec = showsPrec

--------------------------------------------------------------------------------

type GeometryStore' = GeometryStore Identity

newtype GeometryStore f = GeometryStore (DMap.DMap Key f)

deriving newtype instance (Show (DMap.DMap Key f)) => Show (GeometryStore f)
deriving newtype instance Eq   (DMap.DMap Key f) => Eq   (GeometryStore f)

_DMap :: Iso (GeometryStore f) (GeometryStore g) (DMap.DMap Key f) (DMap.DMap Key g)
_DMap = coerced

-- | Creates an empty geometry store
empty :: GeometryStore f
empty = GeometryStore DMap.empty

-- | Inserts a new element in the store (with a new key). Returns the new key and the
-- updated store.
insertNew         :: f v -> GeometryStore f -> (Key v, GeometryStore f)
insertNew x store = (k, store&_DMap %~ DMap.insert k x)
  where
    k = case DMap.lookupMax $ store^._DMap of
          Nothing            -> Key 0
          Just (Key m :=> _) -> Key (m+1)

-- | Insert a new element into the GeometryStore
insertNew'   :: v -> GeometryStore' -> (Key v, GeometryStore')
insertNew' x = insertNew (Identity x)
