module HGeometry.Vector.Additive.Fractional
  ( (^/)
  , module HGeometry.Vector.Additive
  ) where

import R --Fractional
import Vector
import HGeometry.Vector.Additive

--------------------------------------------------------------------------------

infixl 7 ^/

--------------------------------------------------------------------------------
-- * Fractional Additive functionality that requires R to be Fractional

-- | scalar division
(^/)   :: Vector -> R -> Vector
v ^/ s = v ^* (1/s)
{-# INLINE (^/) #-}