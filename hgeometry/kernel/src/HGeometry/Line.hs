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
  , module HGeometry.Line.General
  , LineLineIntersection, LineLineIntersectionG(..)
  , fromLineEQ
  ) where

import Control.Lens
import HGeometry.Intersection
import HGeometry.Line.Class
import HGeometry.Line.Intersection
import HGeometry.Line.LineEQ
import HGeometry.Line.PointAndVector
import HGeometry.Point
import HGeometry.Vector
import HGeometry.Line.General

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

instance (Ord r, Fractional r)
         => IsIntersectableWith (LineEQ r) (LinePV 2 r) where
  l `intersect` m = case toLinearFunction m of
    Nothing    -> let x = m^.anchorPoint.xCoord
                  in Just . Line_x_Line_Point $ Point2 x (evalAt' x l)
                  -- m is vertical, l is not, so they intersect in a point
    Just m'    -> l `intersect` m'

-- | Convert from a LineEQ to a Point and Line
fromLineEQ              :: Num r => LineEQ r -> LinePV 2 r
fromLineEQ (LineEQ a b) = fromLinearFunction a b

instance Fractional r => HasSquaredEuclideanDistance (LineEQ r) where
  pointClosestTo q l = pointClosestTo q (fromLineEQ l)
