--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Line
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Lines in d-dimensional space.
--
--------------------------------------------------------------------------------
module HGeometry.Line
  ( module HGeometry.Line.Class
  , pattern Line
  ) where

import HGeometry.Line.Class
import HGeometry.HyperPlane.NonVertical
import HGeometry.Vector

--------------------------------------------------------------------------------

-- | Constructs a line in R^2, i.e. a line for the equation \(y = ax + b\)
pattern Line     :: OptCVector_ 2 r => r -> r -> NonVerticalHyperPlane 2 r
pattern Line a b = NonVerticalHyperPlane (Vector2 a b)
{-# COMPLETE Line #-}
