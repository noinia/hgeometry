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

  , Disk_(..), pattern Disk_
  ) where

import           Control.Arrow ((&&&))
import           Control.Lens
import           Data.Ord (comparing)
import           GHC.TypeLits
import           HGeometry.Interval.Class
import qualified HGeometry.Number.Radical as Radical
import           HGeometry.Point
import           HGeometry.Properties
import           HGeometry.Vector

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

--------------------------------------------------------------------------------

-- | Constructs a ball from d+1 points on the boundary
fromBoundaryPoints :: Vector (d+1) point -> ball
fromBoundaryPoints = undefined


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


-- | Constructs a Disk by its center and squared radius.
pattern Disk_     :: Disk_ ball point => point -> NumType ball -> ball
pattern Disk_ c r = Ball_ c r
