{-# LANGUAGE FlexibleContexts #-}
module Data.Geometry.LowDimensional where


import Control.Lens(Lens')

import Data.Vinyl.Universe.Geometry

import Data.Geometry.Point


import GHC.TypeLits

--------------------------------------------------------------------------------
-- | Some common fields

-- | Some hands for the axis (fields) in the first three dimensions
x = SNatField :: SDField 1
y = SNatField :: SDField 2
z = SNatField :: SDField 3

_x :: (1 :<= d) => Lens' (Point d fs r) r
_x = _axis x

_y :: (2 :<= d) => Lens' (Point d fs r) r
_y = _axis y

_z :: (3 :<= d) => Lens' (Point d fs r) r
_z = _axis z
