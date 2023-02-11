module HGeometry.Vector.Helper
  ( xComponent
  , yComponent
  , zComponent
  ) where

import Control.Lens
import D
import Data.Type.Ord
import R
import Vector

--------------------------------------------------------------------------------

-- | Access the x-coordinate
xComponent :: (0 < D) => IndexedLens' Int Vector R
xComponent = component @0

-- | Access the y-coordinate
yComponent :: (1 < D) => IndexedLens' Int Vector R
yComponent = component @1

-- | Access the z-coordinate
zComponent :: (2 < D) => IndexedLens' Int Vector R
zComponent = component @2
