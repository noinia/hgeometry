module Data.Geometry.Properties where

--------------------------------------------------------------------------------
import GHC.TypeLits

-- | A type family for types that are associated with a dimension.
type family Dimension t :: Nat

-- | A type family for types that have an associated numeric type.
type family NumType t :: *
