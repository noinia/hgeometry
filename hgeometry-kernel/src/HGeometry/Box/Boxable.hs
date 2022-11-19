{-# LANGUAGE UndecidableInstances #-}
module HGeometry.Box.Boxable
  ( IsBoxable(..)
  ) where

import HGeometry.Box.Class
import HGeometry.Box.Optimal
import HGeometry.Point
import HGeometry.Properties
import HGeometry.Vector

--------------------------------------------------------------------------------

-- | Types for which we can compute an axis parallel boundingbox
class IsBoxable g where
  -- | Compute the axis-parallel boundingbox of the given geometry.
  boundingBox :: g -> Box (Point (Dimension g) (NumType g))

instance ( Box_ (Box point) point
         , Point_ point d r
         , OptVector_ d r
         , OptCVector_ 2 point
         , OptCVector_ 2 (Point d r)
         , Metric_ (VectorFamily' d r)
         ) => IsBoxable (Box point) where
  boundingBox (Box p q) = Box (pointFromPoint p) (pointFromPoint q)
