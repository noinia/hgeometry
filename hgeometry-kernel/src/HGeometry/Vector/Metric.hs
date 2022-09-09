module HGeometry.Vector.Metric
  ( Metric_(..)
  ) where

import           Control.Lens
import           Data.Radical
import           HGeometry.Properties
import           HGeometry.Vector.Additive
import           Prelude hiding (sqrt)

--------------------------------------------------------------------------------

-- | The equivalent class of Linear.Metric
--
-- Note that we do not define a distance itself, and that norm and
-- signorm have a Radical constraint rather than Floating.
class Additive_ vector => Metric_ vector where
  -- | Compute the inner product of two vectors or (equivalently)
  -- convert a vector f a into a covector f a -> a.
  dot :: Num (NumType vector) => vector -> vector -> NumType vector
  dot u v = sumOf components $ liftI2 (*) u v

  -- | Compute the squared norm. The name quadrance arises from Norman
  -- J. Wildberger's rational trigonometry.
  quadrance   :: Num (NumType vector) => vector -> NumType vector
  quadrance v = dot v v

  -- | Compute the quadrance of the difference
  qd     :: Num (NumType vector) => vector -> vector -> NumType vector
  qd u v = quadrance $ u ^-^ v

  -- -- | Compute the distance between two vectors in a metric space
  -- distance :: Radical (NumType vector) => vector -> vector -> NumType vector

  -- | Compute the norm of a vector in a metric space
  norm :: Radical (NumType vector) => vector -> NumType vector
  norm = sqrt . quadrance

  -- | Convert a non-zero vector to unit vector.
  signorm   :: (Radical (NumType vector), Fractional (NumType vector)) => vector -> vector
  signorm v = v ^/ norm v
  {-# MINIMAL #-}
