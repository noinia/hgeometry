{-# LANGUAGE UndecidableInstances  #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Plane.LowerEnvelope.Clipped
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- A Representation of the Lower envelope of planes inside a given triangle. This means
-- now all regions are actually bounded.
--
--------------------------------------------------------------------------------
module HGeometry.Plane.LowerEnvelope.Clipped
  ( ClippedMinimizationDiagram
  , ClippedMinimizationDiagram'
  , _ClippedMinimizationDiagramMap
  , ClippedMDCell, ClippedMDCell'
  , ClippedMDCell''(..)
  , lowerEnvelopeIn
  ) where

import Data.Foldable1
import HGeometry.HyperPlane
import HGeometry.Plane.LowerEnvelope.Clipped.Type
import HGeometry.Plane.LowerEnvelope.Connected.BruteForce
import HGeometry.Point.Class
import HGeometry.Triangle

--------------------------------------------------------------------------------

-- | Compute the lower envelope of planes inside a given triangle.
--
-- \(O(n^4)\) (as it currently uses the brute force algorithm).
lowerEnvelopeIn :: forall plane r corner set.
                   ( Plane_ plane r, Ord plane, Ord r, Fractional r
                   , Point_ corner 2 r
                   , Foldable1 set
                   , Show r, Show corner, Show plane -- TODO: remove these
                   )
                => Triangle corner
                -> set plane
                -> ClippedMinimizationDiagram plane
lowerEnvelopeIn = bruteForceLowerEnvelopeIn
