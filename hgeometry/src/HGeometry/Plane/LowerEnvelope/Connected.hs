--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Plane.LowerEnvelope.Connected
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- A Representation of the Lower envelope of planes as a bunch of convex regions
--
--------------------------------------------------------------------------------
module HGeometry.Plane.LowerEnvelope.Connected
  ( MinimizationDiagram(..)
  , asMap
  , Region(..)
  , module HGeometry.Plane.LowerEnvelope.Connected.Primitives
  , module HGeometry.Plane.LowerEnvelope.Connected.Regions
  , module HGeometry.Plane.LowerEnvelope.Connected.BruteForce
  ) where

import HGeometry.Plane.LowerEnvelope.Connected.BruteForce
import HGeometry.Plane.LowerEnvelope.Connected.Primitives
import HGeometry.Plane.LowerEnvelope.Connected.Regions
import HGeometry.Plane.LowerEnvelope.Connected.Type
