{-# LANGUAGE DeriveFunctor #-}
module Data.Geometry.Triangle where

import Control.Lens
import Data.Ext
import Data.Geometry.Point
import Data.Geometry.Properties
import Data.Geometry.Transformation

data Triangle p r = Triangle (Point 2 r :+ p)
                             (Point 2 r :+ p)
                             (Point 2 r :+ p)
                    deriving (Show,Eq,Functor)

type instance NumType   (Triangle p r) = r
type instance Dimension (Triangle p r) = 2

instance PointFunctor (Triangle p) where
  pmap f (Triangle p q r) = Triangle (p&core %~ f) (q&core %~ f) (r&core %~ f)

instance Num r => IsTransformable (Triangle d r) where
  transformBy = transformPointFunctor


-- | Compute the area of a triangle
area   :: Fractional r => Triangle p r -> r
area t = doubleArea t / 2

-- | 2*the area of a triangle.
doubleArea                  :: Num r => Triangle p r -> r
doubleArea (Triangle a b c) = abs $ ax*by - ax*cy
                                  + bx*cy - bx*ay
                                  + cx*ay - cx*by
                                  -- Based on determinant of a 3x3 matrix (shoelace formula)
  where
    Point2 ax ay = a^.core
    Point2 bx by = b^.core
    Point2 cx cy = c^.core
