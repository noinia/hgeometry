module HGeometry.Line.NonVertical.Class
  ( NVLine2_
  , pattern NVLine2_
  ) where

import HGeometry.HyperPlane.Class
import HGeometry.Line.Class
import HGeometry.Vector

--------------------------------------------------------------------------------

-- | Typeclass modelling Non-vertical lines
type NVLine2_ line r = (Line_ line 2 r, NonVerticalHyperPlane_ line 2 r)

-- | Match on a non-vertical line
pattern NVLine2_     :: NVLine2_ line r => r -> r -> line
pattern NVLine2_ a b <- (hyperPlaneCoefficients -> Vector2 a b)
{-# COMPLETE NVLine2_ #-}
