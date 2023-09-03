module HGeometry.Point.Type
  ( Point
  , PointF(..)
  ) where

import HGeometry.Point.PointF
import HGeometry.Vector

-- | Defines our point type
type Point d r = PointF (Vector d r)
