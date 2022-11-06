module HGeometry.Point.Optimal
  ( Point, pattern Point1, pattern Point2, pattern Point3, pattern Point4
  , PointF(..)
  ) where

import HGeometry.Point.PointF
import HGeometry.Vector

--------------------------------------------------------------------------------

-- | Optimized point type
type Point d r = PointF (Vector d r)

--------------------------------------------------------------------------------
-- * Convenience functions to construct 1, 2 and 3 dimensional points

-- | A bidirectional pattern synonym for 1 dimensional points.
pattern Point1   :: r -> Point 1 r
pattern Point1 x = Point (Vector1 x)
{-# COMPLETE Point1 #-}

-- | A bidirectional pattern synonym for 2 dimensional points.
pattern Point2       :: ConstructableVector_ (VectorFamily 2 r) 2 r => r -> r -> Point 2 r
pattern Point2 x y = Point (Vector2 x y)
{-# COMPLETE Point2 #-}

-- | A bidirectional pattern synonym for 3 dimensional points.
pattern Point3       :: ConstructableVector_ (VectorFamily 3 r) 3 r => r -> r -> r -> Point 3 r
pattern Point3 x y z = (Point (Vector3 x y z))
{-# COMPLETE Point3 #-}

-- | A bidirectional pattern synonym for 4 dimensional points.
pattern Point4         :: ConstructableVector_ (VectorFamily 4 r) 4 r
                       => r -> r -> r -> r -> Point 4 r
pattern Point4 x y z w = (Point (Vector4 x y z w))
{-# COMPLETE Point4 #-}
