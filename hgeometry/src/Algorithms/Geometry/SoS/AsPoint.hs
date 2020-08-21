module Algorithms.Geometry.SoS.AsPoint where

import           Control.CanAquire
import           Data.Ext
import           Data.Geometry.Point.Internal
import           Data.Geometry.Properties
import           Data.Geometry.Vector

--------------------------------------------------------------------------------
-- | a P is a 'read only' point in d dimensions
newtype P i d r = P i deriving (Eq, Show)

-- | Indxec type that can disambiguate points
newtype SoSIndex i = SoSIndex i deriving (Show,Eq,Ord)

instance HasIndex (P i d r) i where
  indexOf (P i) = i

instance Int `CanAquire` (Point d r) => (P Int d r) `CanAquire` (Point d r) where
  aquire (P i) = aquire i

type instance NumType   (P i d r) = r
type instance Dimension (P i d r) = d

asPointWithIndex       :: (Arity d, i `CanAquire` Point d r)
                       => P i d r -> Point d r :+ SoSIndex i
asPointWithIndex (P i) = aquire i :+ (SoSIndex i)
