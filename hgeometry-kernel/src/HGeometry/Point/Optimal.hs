module HGeometry.Point.Optimal
  ( Point
  ) where

import HGeometry.Point.PointF
import HGeometry.Vector

--------------------------------------------------------------------------------

-- | Optimzied point type
type Point d r = PointF (Vector d r)
