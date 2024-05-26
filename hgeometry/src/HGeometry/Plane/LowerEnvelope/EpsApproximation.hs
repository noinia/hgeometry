module HGeometry.LowerEnvelope.EpsApproximation
  (

  ) where

import Control.Monad.State.Class
import HGeometry.HyperPlane.Class
import HGeometry.HyperPlane.NonVertical
import System.Random.Stateful
import Witherable

--------------------------------------------------------------------------------

-- | Given a value r, and a set of planes H, we construct a C/r-approximation A of H w.r.t
-- the shallow downward ranges.
epsApproximation :: ( Plane_ plane r
                    , Ord r, Fractional r, Foldable f, Functor f, Ord plane
                    )
                 => Int -- ^ the parameter r
                 -> f plane -> f plane
epsApproximation = undefined

-- | Given a value r, and a set of planes H, we construct a C/r-approximation A of H w.r.t
-- the shallow downward ranges.
epsApproximation'      :: ( Plane_ plane r
                          , Ord r, Fractional r, Foldable f, Functor f, Ord plane
                          , RandomGen gen, MonadState gen m
                          , Show plane, Show r
                          )
                       => Int -- ^ the parameter r
                       -> f plane -> m (f plane)
epsApproximation' r hs = undefined
