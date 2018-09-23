{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Geometry.Triangle where

import Control.Lens
import Data.Bifunctor
import Data.Ext
import Data.Geometry.Ball (Disk, disk)
import Data.Geometry.Boundary
import Data.Geometry.HyperPlane
import Data.Geometry.LineSegment
import Data.Geometry.Point
import Data.Geometry.Properties
import Data.Geometry.Transformation
import Data.Geometry.Vector
import GHC.TypeLits

--------------------------------------------------------------------------------

data Triangle d p r = Triangle (Point d r :+ p)
                               (Point d r :+ p)
                               (Point d r :+ p)

deriving instance (Arity d, Show r, Show p) => Show (Triangle d p r)
deriving instance (Arity d, Read r, Read p) => Read (Triangle d p r)
deriving instance (Arity d, Eq r, Eq p)     => Eq (Triangle d p r)

instance Arity d => Functor (Triangle d p) where
  fmap f (Triangle p q r) = let f' = first (fmap f) in Triangle (f' p) (f' q) (f' r)


type instance NumType   (Triangle d p r) = r
type instance Dimension (Triangle d p r) = d

instance PointFunctor (Triangle d p) where
  pmap f (Triangle p q r) = Triangle (p&core %~ f) (q&core %~ f) (r&core %~ f)

instance (Fractional r, Arity d, Arity (d + 1)) => IsTransformable (Triangle d p r) where
  transformBy = transformPointFunctor


-- | convenience function to construct a triangle without associated data.
triangle'       :: Point d r -> Point d r -> Point d r -> Triangle d () r
triangle' p q r = Triangle (ext p) (ext q) (ext r)


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


-- | Given a point q and a triangle, q inside the triangle, get the baricentric
-- cordinates of q
toBarricentric                                 :: Fractional r
                                               => Point 2 r -> Triangle 2 p r
                                               -> Vector 3 r
toBarricentric (Point2 qx qy) (Triangle a b c) = Vector3 alpha beta gamma
  where
    Point2 ax ay = a^.core
    Point2 bx by = b^.core
    Point2 cx cy = c^.core

    dett  = (by - cy)*(ax - cx) + (cx - bx)*(ay - cy)

    alpha = ((by - cy)*(qx - cx) + (cx - bx)*(qy - cy)) / dett
    beta  = ((cy - ay)*(qx - cx) + (ax - cx)*(qy - cy)) / dett
    gamma = 1 - alpha - beta
    -- see https://en.wikipedia.org/wiki/Barycentric_coordinate_system#Conversion_between_barycentric_and_Cartesian_coordinates

-- | Given a vector of barricentric coordinates and a triangle, get the
-- corresponding point in the same coordinate sytsem as the vertices of the
-- triangle.
fromBarricentric                                  :: (Arity d, Num r)
                                                  => Vector 3 r -> Triangle d p r
                                                  -> Point d r
fromBarricentric (Vector3 a b c) (Triangle p q r) = let f = view (core.vector) in
    Point $ a *^ f p ^+^ b *^ f q ^+^ c *^ f r


-- | Tests if a point lies inside a triangle, on its boundary, or outside the triangle
inTriangle     :: (Ord r, Fractional r)
                 => Point 2 r -> Triangle 2 p r -> PointLocationResult
inTriangle q t
    | all (`inRange` (OpenRange   0 1)) [a,b,c] = Inside
    | all (`inRange` (ClosedRange 0 1)) [a,b,c] = OnBoundary
    | otherwise                                 = Outside
  where
    Vector3 a b c = toBarricentric q t

-- | Test if a point lies inside or on the boundary of a triangle
onTriangle       :: (Ord r, Fractional r)
                 => Point 2 r -> Triangle 2 p r -> Bool
q `onTriangle` t = let Vector3 a b c = toBarricentric q t
                   in all (`inRange` (ClosedRange 0 1)) [a,b,c]
