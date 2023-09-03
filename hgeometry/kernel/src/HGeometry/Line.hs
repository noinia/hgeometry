{-# OPTIONS_GHC -Wno-orphans #-}
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
  , module HGeometry.Line.LineEQ
  , module HGeometry.Line.PointAndVector
  , LineLineIntersection(..)
  ) where

import Control.Lens
import HGeometry.Intersection
import HGeometry.Line.Class
import HGeometry.Line.Intersection
import HGeometry.Line.LineEQ
import HGeometry.Line.PointAndVector
import HGeometry.Point.Class
import HGeometry.Vector

--------------------------------------------------------------------------------


type instance Intersection (LineEQ r) (LinePV 2 r) =
  Maybe (LineLineIntersection (LineEQ r))

instance (Eq r, Num r) => HasIntersectionWith (LineEQ r) (LinePV 2 r) where
  (LineEQ a b) `intersects` (LinePV p v) = differentSlopes || sameLine
    where
      differentSlopes = (a* v^.xComponent) /= v^.yComponent
      sameLine        = a*(p^.xCoord) + b == p^.yCoord
      -- if the slopes are equal, but lines go through the same point so they are the same
      -- line, and thus they intersect
  {-# INLINE intersects #-}

instance (Eq r, Fractional r)
         => IsIntersectableWith (LineEQ r) (LinePV 2 r) where
  l `intersect` (LinePV p v) = l `intersect` fromPointAndVec @(LineEQ r) p v
