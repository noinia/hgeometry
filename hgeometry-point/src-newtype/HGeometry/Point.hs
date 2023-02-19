module HGeometry.Point
  ( Point
  , module HGeometry.Point.PointF
  ) where

import HGeometry.Point.Class
import HGeometry.Point.PointF
import HGeometry.Vector

-- | D dimensional points
type Point d r = PointF (Vector d r)
