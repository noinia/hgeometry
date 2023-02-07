module HGeometry.Vector.Metric.Radical
  ( norm
  , signorm
  , module HGeometry.Vector.Metric
  ) where

import           HGeometry.Sigs.R -- .Radical
import           HGeometry.Sigs.Vector
import           HGeometry.Vector.Meric

--------------------------------------------------------------------------------

-- | Compute the norm of a vector in a metric space
norm :: Vector -> R
norm = sqrt . quadrance
{-# INLINE norm #-}

-- | Convert a non-zero vector to unit vector.
signorm   :: Vector -> Vector
signorm v = v ^/ norm v
{-# INLINE signorm #-}
