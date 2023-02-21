{-# LANGUAGE RoleAnnotations #-}
module VectorDef(Vector) where

import Control.Lens
import Data.Kind (Type)
import GHC.TypeLits
import HGeometry.Properties

--------------------------------------------------------------------------------

-- | d-dimensional vectors
data family Vector (d :: Nat) (r :: Type) :: Type

type instance Index     (Vector d r) = Int
type instance IxValue   (Vector d r) = r
type instance NumType   (Vector d r) = r
type instance Dimension (Vector d r) = d
