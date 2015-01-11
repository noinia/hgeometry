{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Geometry.Properties where

--------------------------------------------------------------------------------
import GHC.TypeLits

-- | A type family for types that are associated with a dimension.
type family Dimension t :: Nat

-- | A type family for types that have an associated numeric type.
type family NumType t :: *


class IsIntersectableWith g h where
  data Intersection g h
  intersect :: g -> h -> Intersection g h

  -- | g `intersects` h  <=> The intersection of g and h is non-empty.
  --
  -- The default implementation computes the intersection of g and h,
  -- and uses nonEmptyIntersection to determine if the intersection is
  -- non-empty.
  intersects :: g -> h -> Bool
  g `intersects` h = nonEmptyIntersection $ g `intersect` h

  -- | Helper to implement `intersects`.
  nonEmptyIntersection :: Intersection g h -> Bool
  {-# MINIMAL intersect , nonEmptyIntersection #-}

class IsUnionableWith g h where
  data Union g h
  union :: g -> h -> Union g h
