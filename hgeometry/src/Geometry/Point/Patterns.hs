module Geometry.Point.Patterns where

import Geometry.Point.Class
-- import Geometry.Point.Internal(Point)
import Geometry.Vector
import Control.Lens (view)

--------------------------------------------------------------------------------

-- | A bidirectional pattern synonym for 1 dimensional points.
pattern Point1   :: Point_ point 1 r => r -> point 1 r
pattern Point1 x <- (view asVector -> Vector1 x)
  where
    Point1 x = fromVector (Vector1 x)

-- | A bidirectional pattern synonym for 2 dimensional points.
pattern Point2     :: Point_ point 2 r => r -> r -> point 2 r
pattern Point2 x y <- (view asVector -> Vector2 x y)
  where
    Point2 x y = fromVector (Vector2 x y)


-- | A bidirectional pattern synonym for 3 dimensional points.
pattern Point3       :: Point_ point 3 r => r -> r -> r -> point 3 r
pattern Point3 x y z <- (view asVector -> Vector3 x y z)
  where
    Point3 x y z = fromVector (Vector3 x y z)

-- | A bidirectional pattern synonym for 4 dimensional points.
pattern Point4         :: Point_ point 4 r => r -> r -> r -> r -> point 4 r
pattern Point4 x y z w <- (view asVector -> Vector4 x y z w)
  where
    Point4 x y z w = fromVector (Vector4 x y z w)


-- {-# COMPLETE Point1 :: Point 1 r #-}
-- {-# COMPLETE Point2 :: Point 2 r #-}
-- {-# COMPLETE Point3 :: Point 3 r #-}
-- {-# COMPLETE Point4 :: Point 4 r #-}
