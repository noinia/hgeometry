{-# LANGUAGE RoleAnnotations #-}
module VectorDef
  ( Vector(..)
  , UnpackedVector
  ) where

import Control.Lens
import Data.Kind (Type)
import GHC.TypeLits
import HGeometry.Properties

--------------------------------------------------------------------------------

-- | d-dimensional unpacked vectors
newtype Vector  (d :: Nat) (r :: Type) = MkVector (UnpackedVector d r)
  -- deriving newtype (Show,Eq,Ord,Functor,Foldable,Traversable,)

type instance Index     (Vector d r) = Int
type instance IxValue   (Vector d r) = r
type instance NumType   (Vector d r) = r
type instance Dimension (Vector d r) = d




-- | d-dimensional unpacked vectors
data family UnpackedVector (d :: Nat) (r :: Type) :: Type

type instance Index     (UnpackedVector d r) = Int
type instance IxValue   (UnpackedVector d r) = r
type instance NumType   (UnpackedVector d r) = r
type instance Dimension (UnpackedVector d r) = d
