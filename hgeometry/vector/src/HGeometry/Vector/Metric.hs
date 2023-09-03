--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Vector.Metric
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- typeclass that expresses operations on vectors in a Metric space.
--
--------------------------------------------------------------------------------
module HGeometry.Vector.Metric
  ( Metric_(..)
  ) where

import           Control.Lens
import           HGeometry.Number.Radical
import           HGeometry.Vector.Additive
import qualified Linear.V1 as LinearV1
import qualified Linear.V2 as LinearV2
import qualified Linear.V3 as LinearV3
import qualified Linear.V4 as LinearV4
import           Prelude hiding (sqrt)

--------------------------------------------------------------------------------

-- | The equivalent class of Linear.Metric
--
-- Note that we do not define a distance itself, and that norm and
-- signorm have a Radical constraint rather than Floating.
class Additive_ vector => Metric_ vector where
  {-# MINIMAL #-}

  -- | Compute the inner product of two vectors or (equivalently)
  -- convert a vector f a into a covector f a -> a.
  dot :: Num (IxValue vector) => vector -> vector -> IxValue vector
  dot u v = sumOf components $ liftI2 (*) u v
  {-# INLINE dot #-}

  -- | Compute the squared norm. The name quadrance arises from Norman
  -- J. Wildberger's rational trigonometry.
  quadrance   :: Num (IxValue vector) => vector -> IxValue vector
  quadrance v = dot v v
  {-# INLINE quadrance #-}

  -- | Compute the quadrance of the difference
  qd     :: Num (IxValue vector) => vector -> vector -> IxValue vector
  qd u v = quadrance $ u ^-^ v
  {-# INLINE qd #-}

  -- -- | Compute the distance between two vectors in a metric space
  -- distance :: Radical (IxValue vector) => vector -> vector -> IxValue vector

  -- | Compute the norm of a vector in a metric space
  norm :: Radical (IxValue vector) => vector -> IxValue vector
  norm = sqrt . quadrance
  {-# INLINE norm #-}

  -- | Convert a non-zero vector to unit vector.
  signorm   :: (Radical (IxValue vector), Fractional (IxValue vector)) => vector -> vector
  signorm v = v ^/ norm v
  {-# INLINE signorm #-}

instance Monoid c => Metric_ (Const c a)

instance Metric_ (LinearV1.V1 r)
instance Metric_ (LinearV2.V2 r)
instance Metric_ (LinearV3.V3 r)
instance Metric_ (LinearV4.V4 r)
