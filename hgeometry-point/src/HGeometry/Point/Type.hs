module HGeometry.Point.Type
  ( Point
  , PointF(..)
  , HasPickInteriorPoint(..)
  ) where

import HGeometry.Point.PointF
import HGeometry.Vector


--------------------------------------------------------------------------------

-- | d-dimensional points
type Point d r = PointF (Vector d r)

--------------------------------------------------------------------------------

-- | A Class for geometry types that support returning a point inside them.
class HasPickInteriorPoint geom d r | geom -> r, geom -> d where
  -- | Returns a point in the interior of the object
  pointInteriorTo :: geom -> Point d r

-- -- | A Class for geometry types that support returning a point in or on them.
-- class HasPickPoint geom d r | geom -> r, geom -> d where
--   -- | Returns a point in the interior of the object
--   pointOn :: geom -> Point d r
