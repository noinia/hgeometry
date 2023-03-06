module HGeometry.Matrix.Internal where

import HGeometry.Vector

--------------------------------------------------------------------------------
-- * Helper functions to easily create matrices

-- | Creates a row with zeroes everywhere, except at position i, where the
-- value is the supplied value.
mkRow     :: forall d r. (Num r, Has_ Vector_ d r) => Int -> r -> Vector d r
mkRow i x = generate $ \j -> if i == j then x else 0
