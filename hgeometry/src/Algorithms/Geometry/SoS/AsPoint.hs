{-# LANGUAGE UndecidableInstances #-}
module Algorithms.Geometry.SoS.AsPoint where

import Control.CanAquire
import Data.Ext
import Data.Geometry.Point.Internal
import Data.Geometry.Properties
import Data.Geometry.Vector

--------------------------------------------------------------------------------

-- | P is a pointer to a (Point d r :+ e). It essentially acts as a
-- 'read only' (Point d r :+ e)..
--
-- Note that the Eq and Ord instance only evaluate the
-- equality/ordering of the pointers, not the points themselves!
newtype P i d r e = P (I i (Point d r :+ e)) deriving (Show,Eq,Ord)

-- | We use ints as indices
newtype SoSIndex = SoSIndex Int deriving (Show,Eq,Ord)

type instance NumType   (P i d r e) = r
type instance Dimension (P i d r e) = d

instance CanAquire (I i (Point d r :+ e)) => CanAquire (P i d r e) where
  type AquiredVal (P i d r e) = Point d r :+ e
  aquire (P i) = aquire i

asPointWithIndex         :: (Arity d, CanAquire (P i d r e))
                         => P i d r e -> Point d r :+ SoSIndex
asPointWithIndex p@(P i) = let pt :+ _ = aquire p in pt :+ SoSIndex (fromEnum i)
