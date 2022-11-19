{-# LANGUAGE UndecidableInstances #-}
module HGeometry.Box.Optimal
  ( Box(Box)

  ) where

import Control.Lens
import GHC.Generics
import HGeometry.Box.Class
import HGeometry.Interval
import HGeometry.Point
import HGeometry.Properties
import HGeometry.Vector

--------------------------------------------------------------------------------
-- | D-dimensional boxes.
newtype Box point = MkBox (Vector 2 point) deriving (Generic)

-- | Construct a box
pattern Box           :: OptCVector_ 2 point => point -> point -> Box point
pattern Box minP maxP = MkBox (Vector2 minP maxP)
{-# COMPLETE Box #-}

-- type instance PointFor  (Box point) = point
type instance Dimension (Box point) = Dimension point
type instance NumType   (Box point) = NumType point

instance OptCVector_ 2 point => HasMinPoint (Box point) point where
  minPoint = lens (\(Box p _) -> p) (\(Box _ q) p -> Box p q)

instance OptCVector_ 2 point => HasMaxPoint (Box point) point where
  maxPoint = lens (\(Box _ q) -> q) (\(Box p _) q -> Box p q)

instance ( Affine_ point
         , Point_ point (Dimension point) (NumType point)
         , OptCVector_ 2 (NumType point)
         , OptCVector_ 2 point
         ) => Box_ (Box point) point where
  extent (Box p q) = vZipWith ClosedInterval (p^.vector) (q^.vector)
