--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Point.Orientation
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- orientation tests
--
--------------------------------------------------------------------------------
module HGeometry.Point.Orientation
  ( CCW(CCW,CW,CoLinear)
  , ccw
  , isCoLinear

  , sortAround

  , ccwCmpAroundWith
  , cwCmpAroundWith
  , ccwCmpAround
  , cwCmpAround

  , insertIntoCyclicOrder

  , cmpInDirection
  ) where

import HGeometry.Point.Class
import HGeometry.Point.Orientation.Degenerate
import HGeometry.Vector

-- | Comparison that compares which point is 'larger' in the direction given by
-- the vector u.
cmpInDirection       :: (Num r, Ord r, Point_ point 2 r)
                     => Vector 2 r -> point -> point -> Ordering
cmpInDirection u p q = u `dot` (p .-. q) `compare` 0
