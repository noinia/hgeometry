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
  ( Ball_(..)
  , ConstructableBall_(..)
  , pattern Ball_
  , HasCenter(..)
  , radius

  , Disk_ -- (..)
  , pattern Disk_

  , HasInBall(..)
  , inDisk
  ) where

import           Control.Arrow ((&&&))
import           Control.Lens

import           HGeometry.Boundary
import qualified HGeometry.Number.Radical as Radical
import           HGeometry.Point
import           HGeometry.Properties
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
  {-# MINIMAL squaredRadius #-}
  -- | Getter to access the squared radius of a ball
  squaredRadius :: Getter ball (NumType ball)

-- | A lens to get/set the radius of a Ball
radius :: (Radical.Radical r, Ball_ ball point, NumType ball ~ r) => ball -> r
radius = Radical.sqrt . view squaredRadius

class ( Ball_ ball point
      ) => ConstructableBall_ ball point where
  {-# MINIMAL fromCenterAndSquaredRadius #-}
  -- | Construct a ball from the center and *squared* radius
  fromCenterAndSquaredRadius :: point -> NumType ball -> ball

  -- | A d dimensional unit ball centered at the origin.
  unitBall :: (r ~ NumType ball, d ~ Dimension ball
              , Num r, ConstructablePoint_ point d r) => ball
  unitBall = fromCenterAndSquaredRadius origin 1


-- | Constructs a Ball from its center and squared radius.
pattern Ball_     :: ConstructableBall_ ball point => point -> NumType ball -> ball
pattern Ball_ c r <- ((view center &&& view squaredRadius) -> (c,r))
  where
    Ball_ c r = fromCenterAndSquaredRadius c r
{-# COMPLETE Ball_ #-}

--------------------------------------------------------------------------------

-- | Class for types that have an "in ball" test.
class HasInBall ball where
  -- | Test if a query point lies inside or outside of a ball.
  inBall :: ( Point_ point d r, Ord r, Num r
            , NumType ball ~ r, Dimension ball ~ d
            ) => point -> ball -> PointLocationResult

-- | In Disk test
inDisk :: ( Point_ point 2 r, HasInBall disk, Ord r, Num r
          , Dimension disk ~ 2, NumType disk ~ r
          )
       => point -> disk -> PointLocationResult
inDisk = inBall

--------------------------------------------------------------------------------


-- -- | Constructs a ball from d+1 points on the boundary
-- fromBoundaryPoints :: Vector (d+1) point -> ball
-- fromBoundaryPoints = undefined





--------------------------------------------------------------------------------

-- | Disks are two dimensional balls
class (Ball_ disk point, Dimension disk ~ 2) => Disk_ disk point where


-- class (Sphere_ disk point, Dimension disk ~ 2) => Circle_ disk point where


-- | Constructs a Disk by its center and squared radius.
pattern Disk_     :: (Disk_ ball point, ConstructableBall_ ball point)
                  => point -> NumType ball -> ball
pattern Disk_ c r = Ball_ c r
{-# COMPLETE Disk_ #-}

-- -- | Constructs a Circle by its center and squared radius.
-- pattern Circle_     :: Circle_ ball point => point -> NumType ball -> ball
-- pattern Circle_ c r = Sphere_ c r

-- -- | Creates a circle from three points on the boundary
-- fromBoundaryPoints :: ( Disk_ disk (Point 2 r)
--                       , Point_ point 2 r
--                       , Fractional r) => Vector 3 point -> disk
-- fromBoundaryPoints (Vector3 (Point2_ px py) (Point2_ qx qy) (Point2_ sx sy)
--                    ) = Disk_ c (squaredEuclideanDist c (Point2 px py))
--   where
--     f  x y = x^2 + y^2
--     fx x y = V3 (f x y) y       1
--     fy x y = V3 x       (f x y) 1

--     ynom   = det33 $ V3 (fy px py) (fy qx qy) (fy sx sy)
--     xnom   = det33 $ V3 (fx px py) (fx qx qy) (fx sx sy)

--     denom  = (2 *) . det33 $ V3 (V3 px py 1) (V3 qx qy 1) (V3 sx sy 1)
--     c      = Point2 (xnom / denom) (ynom / denom)
