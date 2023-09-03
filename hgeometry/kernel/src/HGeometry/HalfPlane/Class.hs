module HGeometry.HalfPlane.Class
  ( HalfPlane_(..)
  ) where

import Control.Lens
import HGeometry.Line.Class

--------------------------------------------------------------------------------

class Line_ (BoundingLine halfPlane r) 2 r
      => HalfPlane_ halfPlane r | halfPlane -> r where
  type BoundingLine halfPlane r

  -- | Lens to access the boundin gline of a halfspace
  boundingLine :: Lens' halfPlane (BoundingLine halfPlane r)
