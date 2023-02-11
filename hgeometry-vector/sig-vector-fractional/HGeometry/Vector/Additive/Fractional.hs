--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Vector.Additive.Fractional
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Additive operations for vectors that have a fractional numtype
--
--------------------------------------------------------------------------------
module HGeometry.Vector.Additive.Fractional
  ( (^/)
  , module HGeometry.Vector.Additive
  ) where

import R
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
