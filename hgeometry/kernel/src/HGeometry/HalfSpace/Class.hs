module HGeometry.HalfSpace.Class
  ( HalfSpace_(..)
  , HalfPlane_
  ) where

import Control.Lens
import HGeometry.HyperPlane.Class
import HGeometry.Properties (NumType, Dimension)
import HGeometry.Sign

--------------------------------------------------------------------------------

-- | Types modelling halfspaces.
class ( HyperPlane_ (BoundingHyperPlane halfSpace d r) d r
      , Dimension halfSpace ~ d, Dimension (BoundingHyperPlane halfSpace d r) ~ d
      , NumType halfSpace ~ r,   NumType (BoundingHyperPlane halfSpace d r) ~ r
      )
       => HalfSpace_ halfSpace d r | halfSpace -> d,
                                     halfSpace -> r where
  type BoundingHyperPlane halfSpace d r

  -- | Access the bounding hyperplane
  boundingHyperPlane :: Lens' halfSpace (BoundingHyperPlane halfSpace d r)

  -- | Lens to access the sign of the halfspace.
  halfSpaceSign :: Lens' halfSpace Sign

-- | Type synonym for halfplanes in R^2
type HalfPlane_ halfPlane r = HalfSpace_ halfPlane 2 r


-- class Line_ (BoundingLine halfPlane r) 2 r
--       => HalfPlane_ halfPlane r | halfPlane -> r where
--   type BoundingLine halfPlane r

--   -- | Lens to access the boundin gline of a halfspace
--   boundingLine :: Lens' halfPlane (BoundingLine halfPlane r)

--   -- | Get the normal vector into the halfplane
--   normalIntoHalfPlane :: halfPlane -> Vector 2 r
