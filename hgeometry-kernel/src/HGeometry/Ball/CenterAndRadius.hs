{-# LANGUAGE UndecidableInstances #-}
module HGeometry.Ball.CenterAndRadius
  ( Ball(Ball,Disk)
  , Disk
  ) where

import           Control.Lens
import           Data.Ord (comparing)
import           GHC.TypeLits
import           HGeometry.Interval.Class
import qualified HGeometry.Number.Radical as Radical
import           HGeometry.Point
import           HGeometry.Properties
import           HGeometry.Vector
import           HGeometry.Ball.Class


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



instance Point_ point 2 (NumType point) => Disk_ (Ball point) point where

-- | Balls in 2D are also known as Disks
type Disk = Ball

-- | Construct a disk
pattern Disk     :: point -> NumType point -> Disk point
pattern Disk c r = Ball c r
