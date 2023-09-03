module HGeometry.Line.NonVertical.Class
  ( NVLine2_
  , pattern NVLine2_
  , slope, intercept
  ) where

import Control.Lens
import HGeometry.HyperPlane.Class
import HGeometry.Line.Class
import HGeometry.Vector

--------------------------------------------------------------------------------

-- $setup
-- >>> import HGeometry.Line.LineEQ


-- | Typeclass modelling Non-vertical lines
type NVLine2_ line r = (Line_ line 2 r, NonVerticalHyperPlane_ line 2 r)

-- | Match on a non-vertical line
pattern NVLine2_     :: NonVerticalHyperPlane_ line 2 r => r -> r -> line
pattern NVLine2_ a b <- (view hyperPlaneCoefficients -> Vector2 a b)
{-# COMPLETE NVLine2_ #-}

-- | Lens to access the slope of a line
--
-- >>> (LineEQ 10 20) ^. slope
-- 10
slope :: NonVerticalHyperPlane_ line 2 r => Lens' line r
slope = hyperPlaneCoefficients . xComponent

-- | Lens to access the intercept (i.e. the value at which it
-- intersects the y-axis) of a line.
--
-- >>> (LineEQ 10 20) ^. intercept
-- 20
intercept :: NonVerticalHyperPlane_ line 2 r => Lens' line r
intercept = hyperPlaneCoefficients . yComponent
