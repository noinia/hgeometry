{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
module HGeometry.Box.Boxable
  ( IsBoxable(..)
  ) where

import Control.Lens
import HGeometry.Box.Class
import HGeometry.Box.Optimal
import HGeometry.Point
import HGeometry.Properties
import HGeometry.Vector

--------------------------------------------------------------------------------

-- | Types for which we can compute an axis parallel boundingbox
class IsBoxable g where
  -- | Compute the axis-parallel boundingbox of the given geometry.
  boundingBox :: Ord (NumType g) => g -> Box (Point (Dimension g) (NumType g))


-- defaultBBox  :: forall g d r. ( d ~ Dimension g, r ~ NumType g
--                                    -- , HasPoints g g point (Point d r)
--                 , OptVector_ d r
--                 , Metric_ (VectorFamily' d r)
--                 , Ord (VectorFamily' d r)
--                 -- , Point_ point d r
--                 )
--               => g -> Box (Point d r)
-- defaultBBox g = Box bl tr
--     where
--       thePoints :: Fold g (Point d r)
--       thePoints = allPoints.traverse.to pointFromPoint
--       bl = minimum1Of thePoints g
--       tr = maximum1Of thePoints g

instance ( Box_ (Box point) point
         , Point_ point d r
         , OptVector_ d r
         , OptCVector_ 2 point
         , OptCVector_ 2 (Point d r)
         , Metric_ (VectorFamily' d r)
         ) => IsBoxable (Box point) where
  boundingBox (Box p q) = Box (pointFromPoint p) (pointFromPoint q)
