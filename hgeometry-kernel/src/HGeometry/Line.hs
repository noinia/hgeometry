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
import HGeometry.Line.PointAndVector hiding (liesAbove, liesBelow)
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
    VerticalLineThrough x -> Just . Line_x_Line_Point $ Point2 x (evalAt' x l)
                             -- m is vertical, l is not, so they intersect in a point
    NonVertical m'        -> l `intersect` m'

----------------------------------------
-- the symmetric instance

type instance Intersection (LinePV 2 r) (LineEQ r) =
  Maybe (LineLineIntersection (LinePV 2 r))

instance (Eq r, Num r) => HasIntersectionWith (LinePV 2 r) (LineEQ r) where
  linePV `intersects` lineEQ = lineEQ `intersects` linePV
  {-# INLINE intersects #-}

instance (Ord r, Fractional r)
         => IsIntersectableWith (LinePV 2 r) (LineEQ r) where
  linePV `intersect` lineEQ = (linePV <$) <$> lineEQ `intersect` linePV
   -- we use the LineEQ x LinePV instance to compute the intersection. If
   -- the lines are the same; then we just replace the line in the LineLineIntersection
   -- by the linePV

--------------------------------------------------------------------------------

type instance Intersection (VerticalOrLineEQ r) (LinePV 2 r) =
  Maybe (LineLineIntersection (VerticalOrLineEQ r))

instance (Eq r, Num r) => HasIntersectionWith (VerticalOrLineEQ r) (LinePV 2 r) where
  line `intersects` linePV = case line of
    VerticalLineThrough x -> linePV^.direction.xComponent /= 0 ||
                             linePV^.anchorPoint.xCoord == x
    NonVertical lineEQ    -> lineEQ `intersects` linePV
  {-# INLINE intersects #-}

instance (Ord r, Fractional r)
         => IsIntersectableWith (VerticalOrLineEQ r) (LinePV 2 r) where
  line `intersect` linePV = line `intersect` toLinearFunction linePV
  {-# INLINE intersect #-}

----------------------------------------
-- * the symmetric instances

type instance Intersection (LinePV 2 r) (VerticalOrLineEQ r) =
  Maybe (LineLineIntersection (LinePV 2 r))

instance (Eq r, Num r) => HasIntersectionWith (LinePV 2 r) (VerticalOrLineEQ r) where
  linePV `intersects` lineEQ = lineEQ `intersects` linePV
  {-# INLINE intersects #-}

instance (Ord r, Fractional r)
         => IsIntersectableWith (LinePV 2 r) (VerticalOrLineEQ r) where
  linePV `intersect` lineEQ = (linePV <$) <$> lineEQ `intersect` linePV
   -- we use the LineEQ x LinePV instance to compute the intersection. If
   -- the lines are the same; then we just replace the line in the LineLineIntersection
   -- by the linePV

--------------------------------------------------------------------------------


-- | Convert from a LineEQ to a Point and Line
fromLineEQ              :: Num r => LineEQ r -> LinePV 2 r
fromLineEQ (LineEQ a b) = fromLinearFunction a b

instance Fractional r => HasSquaredEuclideanDistance (LineEQ r) where
  pointClosestTo q l = pointClosestTo q (fromLineEQ l)
