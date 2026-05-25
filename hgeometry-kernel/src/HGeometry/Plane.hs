--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Plane
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Non-vertical planes in R^3
--
--------------------------------------------------------------------------------
module HGeometry.Plane
  ( Plane
  , NonVerticalHyperPlane(NonVerticalHyperPlane, Plane)
  , projectedIntersectionLine
  , module HGeometry.HyperPlane.Class
  ) where


import HGeometry.HyperPlane.Class
import HGeometry.HyperPlane.Intersection
import HGeometry.HyperPlane.NonVertical
import HGeometry.Line.General

--------------------------------------------------------------------------------

-- | Given two planes, computes the downward projection of line in
-- which they intersect.
projectedIntersectionLine      :: (Plane_ plane r, Fractional r, Eq r)
                               => plane -> plane
                               -> Maybe (VerticalOrLineEQ r)
projectedIntersectionLine h h' = do Plane_x_Plane_Line l <- planePlaneIntersection h h'
                                    pure l
