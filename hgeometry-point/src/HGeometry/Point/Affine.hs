--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Point.Affine
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Affine operations
--
--------------------------------------------------------------------------------
module HGeometry.Point.Affine
  ( (.-.)
  , (.+^)
  , (.-^)
  ) where

import HGeometry.Vector
import Point
import Point.Affine
import Vector.Additive

--------------------------------------------------------------------------------

-- | subtract a vector from a point
--
-- >>> myPoint .-^ Vector3 100 200 300
-- Point3 (-99) (-198) (-297)
(.-^)   :: Point -> Vector -> Point
p .-^ v = p .+^ negated v
