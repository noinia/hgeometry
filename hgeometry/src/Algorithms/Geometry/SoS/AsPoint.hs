module Algorithms.Geometry.SoS.AsPoint where

import           Algorithms.Geometry.SoS.RWithIdx
import           Control.CanAquire
import           Control.Lens
import           Data.Ext
import           Data.Geometry.Point.Internal
import           Data.Geometry.Properties
import           Data.Geometry.Vector

--------------------------------------------------------------------------------

class AsPoint p where
  asPoint :: p -> Point (Dimension p) (NumType p)

instance AsPoint (Point d r) where
  asPoint = id

instance AsPoint p => AsPoint (p :+ e) where
  asPoint = asPoint . view core
  {-# INLINE asPoint #-}

--------------------------------------------------------------------------------

-- | a P is a 'read only' point in d dimensions
newtype P i d r = P i deriving (HasIndex, Eq, Show)

instance i `CanAquire` (Point d r) => (P i d r) `CanAquire` (Point d r) where
  aquire (P i) = aquire i

type instance NumType   (P i d r) = r
type instance Dimension (P i d r) = d


instance i `CanAquire` (Point d r) => AsPoint (P i d r) where
  asPoint (P i) = aquire i

asPointWithIndex       :: (Arity d, i `CanAquire` Point d r, HasIndex i)
                       => P i d r -> Point d (RWithIdx r)
asPointWithIndex (P i) = Point . imap (\j r -> RWithIdx r (indexOf i) j) . toVec $ aquire i
