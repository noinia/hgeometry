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


class HasCenter geom point | geom -> point where
  -- | Lens to access the center point of a geometry
  center :: Lens' geom point


class ( HasCenter ball point
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


pattern Ball_     :: Ball_ ball point => point -> NumType ball -> ball
pattern Ball_ c r <- ((view center &&& view squaredRadius) -> (c,r))
  where
    Ball_ c r = fromCenterAndSquaredRadius c r

--------------------------------------------------------------------------------

-- | Constructs a ball from d+1 points on the boundary
fromBoundaryPoints :: Vector (d+1) point -> ball
fromBoundaryPoints = undefined


-- | Given two points on the diameter of the ball, construct a ball.
fromDiametralPair     :: (Fractional r, Point_ point d r, NumType ball ~ r, Ball_ ball point)
                      => point -> point -> ball
fromDiametralPair p q = fromCenterAndPoint (p .+^ ((q .-. p) ^/ 2)) p

-- | Construct a ball given the center point and a point p on the boundary.
fromCenterAndPoint     :: (Num r, Point_ point d r, NumType ball ~ r, Ball_ ball point)
                       => point -> point -> ball
fromCenterAndPoint c p = fromCenterAndSquaredRadius c (squaredEuclideanDist c p)


--------------------------------------------------------------------------------

class (Ball_ disk point, Dimension disk ~ 2) => Disk_ disk point where


pattern Disk_     :: Disk_ ball point => point -> NumType ball -> ball
pattern Disk_ c r = Ball_ c r
