module HGeometry.Vector.Optimal
  ( module HGeometry.Vector.Optimal.Internal
  ) where

import HGeometry.Vector.Optimal.Internal

-- import qualified HGeometry.Vector.Optimal.V2 as V2
import qualified HGeometry.Vector.Unboxed.V2 as V2

--------------------------------------------------------------------------------

-- type instance VectorFamily 2 Int = V2.V2 Int
type instance VectorFamily 2 Int = V2.Vec2
