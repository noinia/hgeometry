--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Vector.Metric
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- D-dimensional vectors storing elements of type R
--
--------------------------------------------------------------------------------
module HGeometry.Vector.Metric
  ( dot
  , quadrance
  , qd
  ) where

import Control.Lens
import HGeometry.Vector.Additive
import R
import Vector

--------------------------------------------------------------------------------

-- | Compute the inner product of two vectors or (equivalently)
-- convert a vector f a into a covector f a -> a.
dot :: Vector -> Vector -> R
dot u v = sumOf components $ liftI2 (*) u v
{-# INLINE dot #-}

-- | Compute the squared norm. The name quadrance arises from Norman
-- J. Wildberger's rational trigonometry.
quadrance   :: Vector -> R
quadrance v = dot v v
{-# INLINE quadrance #-}

-- | Compute the quadrance of the difference
qd     :: Vector -> Vector -> R
qd u v = quadrance $ u ^-^ v
{-# INLINE qd #-}

-- -- | Compute the distance between two vectors in a metric space
-- distance :: Radical (IxValue vector) => vector -> vector -> IxValue vector
