module HGeometry.Matrix.Internal where

import           Control.Lens (set, ix)
import           HGeometry.Vector.Class

--------------------------------------------------------------------------------
-- * Helper functions to easily create matrices

-- | Creates a row with zeroes everywhere, except at position i, where the
-- value is the supplied value.
mkRow     :: forall vector d r.
             ( Num r
             , Vector_ vector d r
             , Additive_ vector
             )
          => Int -> r
          -> vector
mkRow i x = set (ix i) x zero
