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
  , squaredRadius'
  , Disk


  , Sphere(Sphere,Circle,MkSphere)
  , Circle

  , _BallSphere
  , _DiskCircle

  , fromDiametralPair, fromCenterAndPoint
  , fromBoundaryPoints

  , IntersectionOf(..)
  ) where

import Control.Lens
import HGeometry.Ball.BoundaryPoints
import HGeometry.Boundary
import HGeometry.Ball.Class
import HGeometry.Ball.Diametral
import HGeometry.Ext
import HGeometry.HalfLine
import HGeometry.HyperPlane
import HGeometry.Intersection
import HGeometry.Line.PointAndVector
import HGeometry.LineSegment
import HGeometry.Number.Radical
import HGeometry.Point
import HGeometry.Properties (NumType, Dimension)
import HGeometry.Vector
import Prelude hiding (sqrt)

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
  squaredRadius = squaredRadius'

-- | Lens to access the squared radius of a ball
squaredRadius' :: Lens' (Ball point) (NumType point)
squaredRadius' = lens (\(Ball _ r) -> r) (\(Ball c _) r -> Ball c r)


instance Point_ point (Dimension point) (NumType point)
         => ConstructableBall_ (Ball point) point where
  fromCenterAndSquaredRadius = Ball

--------------------------------------------------------------------------------
-- * Point in ball

instance ( Point_ point d r, Ord r, Num r, Has_ Metric_ d r
         ) => HasInBall (Ball point) where
  inBall q (Ball c r) = case squaredEuclideanDist (q^.asPoint) (c^.asPoint) `compare` r of
    LT -> Inside
    EQ -> OnBoundary
    GT -> Outside

type instance Intersection (Point d r) (Ball point) = Maybe (Point d r)

instance ( Point_ point d r, Ord r, Num r, Has_ Metric_ d r
         ) => (Point d r) `HasIntersectionWith` (Ball point) where
  intersects q b = q `inBall` b /= Outside

instance ( Point_ point d r
         , Ord r, Num r
         , Has_ Metric_ d r
         ) => (Point d r) `IsIntersectableWith` (Ball point) where
  intersect q b | q `intersects` b = Just q
                | otherwise        = Nothing


--------------------------------------------------------------------------------
-- * Testing if a line/ ray /linesegment intersects a ball

-- essentially this is all just computing  the squared euclidean distance
-- between the object and the center, and testing if it is at most r

instance ( Point_ point d r
         , Ord r, Fractional r
         , Has_ Metric_ d r
         ) => (LinePV d r) `HasIntersectionWith` (Ball point) where
  intersects l (Ball c r) = squaredEuclideanDistTo c l <= r

instance ( Point_ point d r, Point_ point' d r
         , Ord r, Fractional r
         , Has_ Metric_ d r
         , HasSquaredEuclideanDistance point'
         , MkHyperPlaneConstraints d r
         ) => (ClosedLineSegment point') `HasIntersectionWith` (Ball point) where
  intersects s (Ball c r) = squaredEuclideanDistTo c s <= r

instance ( Point_ point d r, Point_ point' d r
         , Ord r, Fractional r
         , Has_ Metric_ d r
         , HasSquaredEuclideanDistance point'
         , MkHyperPlaneConstraints d r
         ) => (HalfLine point') `HasIntersectionWith` (Ball point) where
  intersects hl (Ball c r) = squaredEuclideanDistTo c hl <= r

----------------------------------------

type instance Intersection (LinePV d r) (Ball point) =
  Maybe (IntersectionOf (LinePV d r) (Ball point))

-- | The extra value is the parameter t, so that the intersection point
-- is line^.anchroPoint + t*line^.direction
data instance IntersectionOf (LinePV d r) (Ball point) =
    Line_x_Ball_Point   (Point d r :+ r)
  | Line_x_Ball_Segment (ClosedLineSegment (Point d r :+ r))
    -- ^ The line segement is oriented from the point closest to the anchor towards
    -- the point further away

deriving instance (Show r, Has_ Additive_ d r) => Show (IntersectionOf (LinePV d r) (Ball point))
deriving instance (Eq r, Eq (Vector d r))      => Eq   (IntersectionOf (LinePV d r) (Ball point))


instance ( Point_ point d r
         , Ord r, Fractional r, Radical r
         , Has_ Metric_ d r
         ) => (LinePV d r) `IsIntersectableWith` (Ball point) where
  intersect (LinePV p v) (Ball c' r) = case discr `compare` 0 of
      LT -> Nothing
      EQ -> Just $ Line_x_Ball_Point q0 -- line touches in q
      GT -> Just $ Line_x_Ball_Segment (ClosedLineSegment q1 q2)
    where
      -- main idea: let q = p + \lambda v be an intersection point, we also have
      -- squaredEuclideanDist q c' == squaredRadius (=r) this yields some quadratic
      -- equation in \lambda, which we just solve using the ABC formula. In particular, we have
      --
      -- (sum_i=1^d (p+\lambda v_i - c'_i)^2 = r)
      -- (sum_i=1^d (\lambda v_i + (p_i- c'_i))^2  - r = 0 )
      -- (sum_i=1^d ((\lambda v_i)^2 + 2\lambda v_i(p_i- c'_i) + (p_i- c'_i)^2)  - r = 0 )
      -- (sum_i=1^d (v_i^2\lambda^2 + 2v_i(p_i- c'_i)\lambda  + (p_i- c'_i)^2)  - r = 0 )
      -- (lambda^2 sum_i=1^d v_i^2\ + \lambda sum_i=1^d 2v_i(p_i- c'_i)  + sum_i=1^d (p_i- c'_i)^2)  - r = 0 )


      a = v `dot` v -- sum_i v_i^2
      b = 2 * (v `dot` u) -- sum_i v_i(p_i-c_i)
      c = (u `dot` u) - r -- sum_i (p_i-c_i)^2  - radius^2

      u = p .-. (c'^.asPoint) -- helper

      discr  = b^2 - 4*a*c
      discr' = sqrt discr
      da = 2*a

      lambda1' = ((negate discr') - b) / da -- the two solutions
      lambda2' = (discr'          - b) / da --
      -- note: v must have non-zero length; and thus a (and therefore da) are non-zero.
      -- note2: if discr is nonzero then lambda1' and laambda2' are distinct

      -- make sure lambda1 is the smaller one. (since they are distinct we can use <
      -- rather than <=).
      (lambda1,lambda2) = if lambda1' < lambda2' then (lambda1',lambda2')
                                                 else (lambda2',lambda1')

      q1 = p .+^ (lambda1 *^ v) :+ lambda1
      q2 = p .+^ (lambda2 *^ v) :+ lambda2

      -- if the discr is zero there is only one solution:
      lambda0 = (negate b) / da
      q0 = p .+^ (lambda0 *^ v) :+ lambda0

----------------------------------------

type instance Intersection (HalfLine point') (Ball point) =
  Maybe (IntersectionOf (LinePV (Dimension point) (NumType point)) (Ball point))

instance ( Point_ point d r, Point_ point' d r
         , Ord r, Fractional r, Radical r
         , Has_ Metric_ d r
         , MkHyperPlaneConstraints d r
         , HasSquaredEuclideanDistance point'
         ) => (HalfLine point') `IsIntersectableWith` (Ball point) where
  intersect (HalfLine p v) b = intersect (LinePV p' v) b >>= \case
      Line_x_Ball_Point q
        | q^.extra >= 0 -> Just $ Line_x_Ball_Point q
        | otherwise     -> Nothing
      Line_x_Ball_Segment seg@(ClosedLineSegment s t) -> case 0 `compare` (t^.extra) of
        GT -> Nothing -- The oriented line intersects the ball before the ray starts
        EQ -> Just $ Line_x_Ball_Point t
                             -- we only intersect at the start of the ray
        LT | s^.extra < 0 -> Just $ Line_x_Ball_Segment (ClosedLineSegment (p' :+ 0) t)
           | otherwise    -> Just $ Line_x_Ball_Segment seg
    where
      p'  = p^.asPoint

    --       -> _


    --     -> case (0 <= s^.extra, 0 <= t^.extra) of
    --     (False,False) -> Nothing
    --     ()

    --     case (a `intersects` h, c `intersects` h) of
    --       (False,False) -> Nothing
    --       (False,True)  -> Just $ Line_x_Ball_Segment (ClosedLineSegment p'' c)
    --       (True,False)  -> Just $ Line_x_Ball_Segment (ClosedLineSegment a  p'')
    --       (True,True)   -> Just $ Line_x_Ball_Segment seg
    -- where
    --   h :: HalfSpace d r
    --   h   = HalfSpace Positive (fromPointAndNormal p' v)
      -- p'' = p' :+ 0

type instance Intersection (ClosedLineSegment point') (Ball point) =
  Maybe (IntersectionOf (LinePV (Dimension point) (NumType point)) (Ball point))

-- data instance IntersectionOf (ClosedLineSegment point) (Ball point) =
--     LineSegment_x_Ball_Point   point
--   | LineSegment_x_Ball_Segment (ClosedLineSegment point)

-- deriving instance (Show point, Show (ClosedLineSegment point))
--                => Show (IntersectionOf (ClosedLineSegment point) (Ball point))
-- deriving instance (Eq point, Eq (ClosedLineSegment point))
--                => Eq (IntersectionOf (ClosedLineSegment point) (Ball point))










--------------------------------------------------------------------------------



instance Point_ point 2 (NumType point) => Disk_ (Ball point) point where

-- | Balls in 2D are also known as Disks
type Disk = Ball

-- | Construct a disk
pattern Disk     :: point -> NumType point -> Disk point
pattern Disk c r = Ball c r
{-# COMPLETE Disk #-}

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


-- | Given two points on the diameter of the ball, construct a ball.
fromDiametralPair     :: (Fractional r, Point_ point d r, Has_ Metric_ d r)
                      => point -> point -> Ball (Point d r)
fromDiametralPair p q = let disk = DiametralPoints p q
                        in fromCenterAndPoint (disk^.center) p

-- | Construct a ball given the center point and a point p on the boundary.
fromCenterAndPoint     :: ( Num r, Point_ point d r, Point_ center d r
                          , Has_ Metric_ d r
                          )
                       => center -> point -> Ball center
fromCenterAndPoint c p = fromCenterAndSquaredRadius c (squaredEuclideanDist c p)

-- | Tries to create a disk from three points on the boundary.
fromBoundaryPoints                 :: ( Point_ point 2 r, Fractional r, Ord r)
                                   => Vector 3 point -> Maybe (Disk (Point 2 r))
fromBoundaryPoints (Vector3 a b d) = diskFromPoints a b d <&> \case
  disk -> let c = disk^.center
          in Ball c (squaredEuclideanDist c a)
