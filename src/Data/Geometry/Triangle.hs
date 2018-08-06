{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Geometry.Triangle where

import Data.Bifunctor
import Control.Lens
import Data.Ext
import Data.Geometry.Point
import Data.Geometry.Vector
import Data.Geometry.HyperPlane
import Data.Geometry.Ball(Disk, disk)
import Data.Geometry.LineSegment
import Data.Geometry.Properties
import Data.Geometry.Transformation
import GHC.TypeLits

--------------------------------------------------------------------------------

data Triangle d p r = Triangle (Point d r :+ p)
                               (Point d r :+ p)
                               (Point d r :+ p)

deriving instance (Arity d, Show r, Show p) => Show (Triangle d p r)

instance Arity d => Functor (Triangle d p) where
  fmap f (Triangle p q r) = let f' = first (fmap f) in Triangle (f' p) (f' q) (f' r)


type instance NumType   (Triangle d p r) = r
type instance Dimension (Triangle d p r) = d

instance PointFunctor (Triangle d p) where
  pmap f (Triangle p q r) = Triangle (p&core %~ f) (q&core %~ f) (r&core %~ f)

instance (Fractional r, Arity d, Arity (d + 1)) => IsTransformable (Triangle d p r) where
  transformBy = transformPointFunctor

sideSegments                  :: Triangle d p r -> [LineSegment d p r]
sideSegments (Triangle p q r) =
  [ClosedLineSegment p q, ClosedLineSegment q r, ClosedLineSegment r p]

-- | Compute the area of a triangle
area   :: Fractional r => Triangle 2 p r -> r
area t = doubleArea t / 2

-- | 2*the area of a triangle.
doubleArea                  :: Num r => Triangle 2 p r -> r
doubleArea (Triangle a b c) = abs $ ax*by - ax*cy
                                  + bx*cy - bx*ay
                                  + cx*ay - cx*by
                                  -- Based on determinant of a 3x3 matrix (shoelace formula)
  where
    Point2 ax ay = a^.core
    Point2 bx by = b^.core
    Point2 cx cy = c^.core


-- | get the inscribed disk. Returns Nothing if the triangle is degenerate,
-- i.e. if the points are colinear.
inscribedDisk                  :: (Eq r, Fractional r)
                               => Triangle 2 p r -> Maybe (Disk () r)
inscribedDisk (Triangle p q r) = disk (p^.core) (q^.core) (r^.core)


instance Num r => HasSupportingPlane (Triangle 3 p r) where
  supportingPlane (Triangle p q r) = from3Points (p^.core) (q^.core) (r^.core)
