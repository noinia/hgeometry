module HGeometry.Point
  ( Point
  , PointF(Point, Point1, Point2, Point3, Point4)
  , module HGeometry.Point.Class
  ) where

import HGeometry.Point.Class
import HGeometry.Point.PointF
import HGeometry.Vector
import R


-- | d dimensional points
type Point d r = PointF (Vector d r)

--------------------------------------------------------------------------------

-- | Construct a 1 dimensional point
pattern Point1   :: R -> Point 1 R
pattern Point1 x = Point (Vector1 x)
{-# COMPLETE Point1 #-}

-- | Construct a 2 dimensional point
pattern Point2     :: R -> R -> Point 2 R
pattern Point2 x y = Point (Vector2 x y)
{-# COMPLETE Point2 #-}

-- | Construct a 3 dimensional point
pattern Point3       :: R -> R -> R -> Point 3 R
pattern Point3 x y z = Point (Vector3 x y z)
{-# COMPLETE Point3 #-}

-- | Construct a 4 dimensional point
pattern Point4         :: R -> R -> R -> R -> Point 4 R
pattern Point4 x y z w = Point (Vector4 x y z w)
{-# COMPLETE Point2 #-}

--------------------------------------------------------------------------------
