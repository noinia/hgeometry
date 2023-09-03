--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Ball.Class
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- A class of types that can act as balls in \(d\)-dimensional space.
--
--------------------------------------------------------------------------------
module HGeometry.Ball.Class
  ( Ball_(..), pattern Ball_
  , HasCenter(..)

  , fromDiametralPair, fromCenterAndPoint

  , Disk_ -- (..)
  , fromBoundaryPoints
  , pattern Disk_
  ) where

import           Control.Arrow ((&&&))
import           Control.Lens
import qualified HGeometry.Number.Radical as Radical
import           HGeometry.Point
import           HGeometry.Properties
import           HGeometry.Vector
import           Linear.Matrix (det33)
import           Linear.V3 (V3(..))

--------------------------------------------------------------------------------

-- | Types that have a 'center' field lens
class HasCenter geom point | geom -> point where
  -- | Lens to access the center point of a geometry
  center :: Lens' geom point

-- | Balls in d-dimensional space
class ( HasCenter ball point
      , NumType ball ~ NumType point
      , Point_ point (Dimension ball) (NumType ball)
      ) => Ball_ ball point | ball -> point where
  {-# MINIMAL squaredRadius, fromCenterAndSquaredRadius #-}

  -- | Lens to access the squared radius of a ball
  squaredRadius :: Lens' ball (NumType ball)

  -- | Construct a ball from the center and *squared* radius
  fromCenterAndSquaredRadius :: point -> NumType ball -> ball

  -- | A d dimensional unit ball centered at the origin.
  unitBall :: Num (NumType ball) => ball
  unitBall = fromCenterAndSquaredRadius origin 1

  -- | A lens to get/set the radius of a Ball
  radius :: Radical.Radical (NumType ball) => Lens' ball (NumType ball)
  radius = lens (Radical.sqrt . view squaredRadius) (\b r -> set squaredRadius (r*r) b)

-- | Constructs a Ball from its center and squared radius.
pattern Ball_     :: Ball_ ball point => point -> NumType ball -> ball
pattern Ball_ c r <- ((view center &&& view squaredRadius) -> (c,r))
  where
    Ball_ c r = fromCenterAndSquaredRadius c r
{-# COMPLETE Ball_ #-}

--------------------------------------------------------------------------------

-- -- | Constructs a ball from d+1 points on the boundary
-- fromBoundaryPoints :: Vector (d+1) point -> ball
-- fromBoundaryPoints = undefined



-- | Given two points on the diameter of the ball, construct a ball.
fromDiametralPair     :: (Fractional r, Point_ point d r, Ball_ ball point, Has_ Metric_ d r)
                      => point -> point -> ball
fromDiametralPair p q = fromCenterAndPoint (p .+^ ((q .-. p) ^/ 2)) p

-- | Construct a ball given the center point and a point p on the boundary.
fromCenterAndPoint     :: ( Num r, Point_ point d r, Ball_ ball point
                          , Has_ Metric_ d r
                          )
                       => point -> point -> ball
fromCenterAndPoint c p = fromCenterAndSquaredRadius c (squaredEuclideanDist c p)


--------------------------------------------------------------------------------

-- | Disks are two dimensional balls
class (Ball_ disk point, Dimension disk ~ 2) => Disk_ disk point where


-- class (Sphere_ disk point, Dimension disk ~ 2) => Circle_ disk point where


-- | Constructs a Disk by its center and squared radius.
pattern Disk_     :: Disk_ ball point => point -> NumType ball -> ball
pattern Disk_ c r = Ball_ c r
{-# COMPLETE Disk_ #-}

-- -- | Constructs a Circle by its center and squared radius.
-- pattern Circle_     :: Circle_ ball point => point -> NumType ball -> ball
-- pattern Circle_ c r = Sphere_ c r

-- | Creates a circle from three points on the boundary
fromBoundaryPoints :: ( Disk_ disk (Point 2 r)
                      , Point_ point 2 r
                      , Fractional r) => Vector 3 point -> disk
fromBoundaryPoints (Vector3 (Point2_ px py) (Point2_ qx qy) (Point2_ sx sy)
                   ) = Disk_ c (squaredEuclideanDist c (Point2 px py))
  where
    f  x y = x^2 + y^2
    fx x y = V3 (f x y) y       1
    fy x y = V3 x       (f x y) 1

    xnom   = det33 $ V3 (fx px py) (fx qx qy) (fx sx sy)
    ynom   = det33 $ V3 (fy px py) (fy qx qy) (fy sx sy)

    denom  = (2 *) . det33 $ V3 (V3 px py 1) (V3 qx qy 1) (V3 sx sy 1)
    c      = Point2 (xnom / denom) (ynom / denom)
