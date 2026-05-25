--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.HyperPlane
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Hyperplanes in \(d\)-dimensional space.
--
--------------------------------------------------------------------------------
module HGeometry.HyperPlane
  ( module HGeometry.HyperPlane.Class
  , HyperPlane(..)
  , MkHyperPlaneConstraints
  , cmpInDirection
  , module HGeometry.HyperPlane.NonVertical
  , PlanePlaneIntersection(..)
  ) where

--------------------------------------------------------------------------------

import HGeometry.HyperPlane.Class
import HGeometry.HyperPlane.Internal
import HGeometry.HyperPlane.NonVertical
import HGeometry.HyperPlane.Intersection
