{-# LANGUAGE DefaultSignatures #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Intersection
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Defines a data type for representing intersections. Mostly useful
-- for the more geometric types.
--
--------------------------------------------------------------------------------
module HGeometry.Intersection
  ( Intersection
  , IntersectionOf
  , HasIntersectionWith(..)
  , IsIntersectableWith(..)
  ) where

import Control.Lens ((^.))
import Data.Kind (Type)
import Data.Maybe (isJust)
import HGeometry.Ext

-------------------------------------------------------------------------------

-- | The result of interesecting two geometries,
type family Intersection g h :: Type

-- | The data family specifying to help implement the 'Intersection'
-- type family.
data family IntersectionOf g h

-- | Class for types for which we can test if they intersect.
class HasIntersectionWith g h where
  -- | g `intersects` h  <=> The intersection of g and h is non-empty.
  intersects :: g -> h -> Bool
  default intersects :: ( Intersection g h ~ Maybe intersection
                        , IsIntersectableWith g h
                        ) => g -> h -> Bool
  g `intersects` h = isJust $ g `intersect` h

-- | Class relationship between intersectable geometric objects.
class HasIntersectionWith g h => IsIntersectableWith g h where
  -- | Computes te intersection of two geometric objects.
  intersect :: g -> h -> Intersection g h


-- type instance Intersection (geomA :+ extra) (geomB :+ extra) = Intersection geomA geomB

instance HasIntersectionWith geomA geomB
         => HasIntersectionWith (geomA :+ extra) (geomB :+ extra) where
  ga `intersects` gb = (ga^.core) `intersects` (gb^.core)

-- instance IsIntersectableWith geomA geomB
--          => IsIntersectableWith (geomA :+ extra) (geomB :+ extra) where
--   ga `intersect` gb = (ga^.core) `intersect` (gb^.core)
