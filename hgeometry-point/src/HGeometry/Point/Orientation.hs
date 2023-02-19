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
  ) where

import HGeometry.Point.Orientation.Degenerate
