{-# LANGUAGE Unsafe #-}
module Data.Geometry.Matrix.Internal where

import           Control.Lens (set)
import           Data.Geometry.Vector
import qualified Data.Vector.Fixed as FV

--------------------------------------------------------------------------------
-- * Helper functions to easily create matrices

-- | Creates a row with zeroes everywhere, except at position i, where the
-- value is the supplied value.
mkRow     :: forall d r. (Arity d, Num r) => Int -> r -> Vector d r
mkRow i x = set (FV.element i) x zero
