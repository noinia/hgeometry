{-# LANGUAGE UndecidableInstances #-}
module Data.Geometry.BezierSpline where

import Data.Ext
import Data.Geometry.Point
import Data.Geometry.Vector

--------------------------------------------------------------------------------

data CubicBezier d r = CubicBezier (Point d r) (Point d r) (Point d r) (Point d r)


deriving instance (Arity d, Show r) => Show (CubicBezier d r)
deriving instance (Arity d, Eq r)   => Eq (CubicBezier d r)

deriving instance Arity d => Functor (CubicBezier d)

-- | Given a 'time' t in [0,1], computes the point at that time.
atTime           :: (Arity d, Num r) => r -> CubicBezier d r -> Point d r
atTime t (CubicBezier (Point p0) (Point p1) (Point p2) (Point p3)) = Point $
    (s*s*s) *^ p0 ^+^ 3*t*s*s *^ p1 ^+^ 3*t*t*s *^ p2 ^+^ t*t*t *^ p3
  where
    s = 1 - t
