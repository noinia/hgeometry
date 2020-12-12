module Algorithms.Geometry.SoS.Internal where

import Algorithms.Geometry.SoS.AsPoint
import Algorithms.Geometry.SoS.Orientation
import Control.CanAquire
import Data.Ext
import Data.Geometry.Point.Internal
-- import Unsafe.Coerce (unsafeCoerce)

--------------------------------------------------------------------------------

-- | The actual implementation of SoS
simulateSimplicity     :: forall t d r b e. (Traversable t, SoS d)
                       => (forall i. ( CanAquire (P i d r e)
                                     , SoS d
                                     ) => t (P i d r e) -> b)
                       -> t (Point d r :+ e) -> b
simulateSimplicity alg = runAcquire alg'
  where
    alg' :: forall s. CanAquire (I s (Point d r :+ e)) => t (I s (Point d r :+ e)) -> b
    alg' = alg . fmap P
    -- alg' = alg . (unsafeCoerce :: t (I s (Point d r :+ e)) -> t (P s d r e))
    -- ideally the fmap would just be a coerce, but GHC does not want to do that.
    -- both I and P are just Ints 'under the hood'. So this should be safe
