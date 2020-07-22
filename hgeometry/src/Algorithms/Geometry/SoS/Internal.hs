module Algorithms.Geometry.SoS.Internal where

import           Algorithms.Geometry.SoS.AsPoint
import           Algorithms.Geometry.SoS.Orientation
import           Control.CanAquire
import           Data.Geometry.Point.Internal

--------------------------------------------------------------------------------

-- simulateSimplicity :: forall t d r b. (Traversable t, SoSD d)
--                    => (forall p. ( AsPoint p, HasIndex p
--                                  , d ~ Dimension p, r ~ NumType p
--                                  ) => t p -> b)
--                    -> t (Point d r) -> b
-- simulateSimplicity = simulateSimplicity'


-- | The actual implementation of SoS
simulateSimplicity'     :: forall t d r b. (Traversable t, SoS d)
                        => (forall i. ( CanAquire i (Point d r)
                                      , SoS d
                                      ) => t (P i d r) -> b)
                        -> t (Point d r) -> b
simulateSimplicity' alg = runAcquire alg'
  where
    alg' :: forall i. CanAquire i (Point d r) => t i -> b
    alg' = alg . fmap (P @i @d @r)
      -- ideally the fmap would just be a coerce, but GHC does not want to do that.
