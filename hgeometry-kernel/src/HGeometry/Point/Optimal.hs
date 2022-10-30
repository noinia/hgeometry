module HGeometry.Point.Optimal
  ( Point
  , PointF(..)
  ) where

import HGeometry.Point.PointF
import HGeometry.Vector

--------------------------------------------------------------------------------

-- | Optimized point type
type Point d r = PointF (Vector d r)
