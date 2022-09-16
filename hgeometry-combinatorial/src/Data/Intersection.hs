{-# LANGUAGE DefaultSignatures #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Intersection
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Defines a data type for representing intersections. Mostly useful
-- for the more geometric types.
--
--------------------------------------------------------------------------------
module Data.Intersection where

import Data.Kind (Type)
import Data.Maybe (isJust)

-------------------------------------------------------------------------------

-- | The result of interesecting two geometries,
type family Intersection g h :: Type

-- | The type family specifying the list of possible result types of an
-- intersection.
data family IntersectionOf g h

class HasIntersectionWith g h where
  -- | g `intersects` h  <=> The intersection of g and h is non-empty.
  intersects :: g -> h -> Bool
  default intersects :: ( Intersection g h ~ Maybe (IntersectionOf g h)
                        , IsIntersectableWith g h
                        ) => g -> h -> Bool
  g `intersects` h = isJust $ g `intersect` h

-- | Class relationship between intersectable geometric objects.
class HasIntersectionWith g h => IsIntersectableWith g h where
  intersect :: g -> h -> Intersection g h
