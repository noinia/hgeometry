{-# LANGUAGE UndecidableInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Ball.CenterAndRadius
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Balls in d-dimensional space, represented by their center and squared radius.
--
--------------------------------------------------------------------------------
module HGeometry.Ball.CenterAndRadius
  ( Ball(Ball,Disk)
  , Disk


  , Sphere(Sphere,Circle,MkSphere)
  , Circle

  , _BallSphere
  , _DiskCircle
  ) where


import           Control.Lens
import           HGeometry.Ball.Class
import           HGeometry.Intersection
import           HGeometry.LineSegment
-- import qualified HGeometry.Number.Radical as Radical
import           HGeometry.Point
import           HGeometry.Properties (NumType, Dimension)
import           HGeometry.Vector


--------------------------------------------------------------------------------

-- | d-dimensional ball, specified by center and squared radius.
data Ball point = Ball !point !(NumType point)

deriving stock instance (Show point, Show (NumType point)) => Show (Ball point)
deriving stock instance (Eq point, Eq (NumType point)) => Eq (Ball point)

type instance NumType   (Ball point) = NumType point
type instance Dimension (Ball point) = Dimension point

instance HasCenter (Ball point) point where
  center = lens (\(Ball c _) -> c) (\(Ball _ r) c -> Ball c r)

instance Point_ point (Dimension point) (NumType point) => Ball_ (Ball point) point where
  squaredRadius = lens (\(Ball _ r) -> r) (\(Ball c _) r -> Ball c r)
  fromCenterAndSquaredRadius = Ball

--------------------------------------------------------------------------------

type instance Intersection (Point d r) (Ball point) = Maybe (Point d r)

instance ( Point_ point d r
         , Ord r, Num r
         , Has_ Metric_ d r
         ) => (Point d r) `HasIntersectionWith` (Ball point) where
  intersects q (Ball c r) = squaredEuclideanDist q (c^.asPoint) <= r

instance ( Point_ point d r
         , Ord r, Num r
         , Has_ Metric_ d r
         ) => (Point d r) `IsIntersectableWith` (Ball point) where
  intersect q b | q `intersects` b = Just q
                | otherwise        = Nothing

-- type instance Intersection (Line point) (Ball point) =
--   Maybe (IntersectionOf (LineSegment point) (Ball point))

data instance IntersectionOf (ClosedLineSegment point) (Ball point) =
    Line_x_Ball_Point   point
  | Line_x_Ball_Segment (ClosedLineSegment point)

deriving instance (Show point, Show (ClosedLineSegment point))
               => Show (IntersectionOf (ClosedLineSegment point) (Ball point))
deriving instance (Eq point, Eq (ClosedLineSegment point))
               => Eq (IntersectionOf (ClosedLineSegment point) (Ball point))


type instance Intersection (ClosedLineSegment point) (Ball point) =
  Maybe (IntersectionOf (ClosedLineSegment point) (Ball point))


instance ( Point_ point d r, Point_ point' d r
         , Ord r, Fractional r
         , Has_ Metric_ d r
         , HasSquaredEuclideanDistance point'
         ) => (ClosedLineSegment point') `HasIntersectionWith` (Ball point) where
  intersects s (Ball c r) = squaredEuclideanDistTo c s <= r



--------------------------------------------------------------------------------



instance Point_ point 2 (NumType point) => Disk_ (Ball point) point where

-- | Balls in 2D are also known as Disks
type Disk = Ball

-- | Construct a disk
pattern Disk     :: point -> NumType point -> Disk point
pattern Disk c r = Ball c r


--------------------------------------------------------------------------------

-- | A sphere, i.e. the boudary of a Ball.
newtype Sphere point = MkSphere (Ball point)

-- | Construct a Sphere; the boundary of a ball
pattern Sphere     :: point -> NumType point -> Sphere point
pattern Sphere c r = MkSphere (Ball c r)
{-# COMPLETE Sphere #-}

-- | A circle, i.e. the boundary of a Disk
type Circle = Sphere

-- | Construct a Circle
pattern Circle     :: point -> NumType point -> Circle point
pattern Circle c r = Sphere c r
{-# COMPLETE Circle #-}


deriving stock instance (Show point, Show (NumType point)) => Show (Sphere point)
deriving stock instance (Eq point, Eq (NumType point)) => Eq (Sphere point)

type instance NumType   (Sphere point) = NumType point
type instance Dimension (Sphere point) = Dimension point

instance HasCenter (Sphere point) point where
  center = lens (\(Sphere c _) -> c) (\(Sphere _ r) c -> Sphere c r)

--------------------------------------------------------------------------------

-- | Iso to convert between a ball and a sphere.
_BallSphere :: Iso (Ball point) (Ball point') (Sphere point) (Sphere point')
_BallSphere = coerced
{-# INLINE _BallSphere #-}

-- | Iso to convert between a Disk and a Circle
_DiskCircle :: Iso (Disk point) (Disk point') (Circle point) (Circle point')
_DiskCircle = _BallSphere
{-# INLINE _DiskCircle #-}

--------------------------------------------------------------------------------

type instance Intersection (Point d r) (Sphere point) = Maybe (Point d r)

instance ( Point_ point d r
         , Eq r, Num r
         , Has_ Metric_ d r
         ) => (Point d r) `HasIntersectionWith` (Sphere point) where
  intersects q (Sphere c r) = squaredEuclideanDist q (c^.asPoint) == r

instance ( Point_ point d r
         , Eq r, Num r
         , Has_ Metric_ d r
         -- , OptVector_ d r, OptMetric_ d r
         ) => (Point d r) `IsIntersectableWith` (Sphere point) where
  intersect q b | q `intersects` b = Just q
                | otherwise        = Nothing

instance ( Point_ point d r, Point_ point' d r
         , Ord r, Fractional r
         , Has_ Metric_ d r
         , HasSquaredEuclideanDistance point'
         ) => (ClosedLineSegment point') `HasIntersectionWith` (Sphere point) where
  intersects s (Sphere c r) = squaredEuclideanDistTo c s <= r
