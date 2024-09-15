{-# LANGUAGE UndecidableInstances #-}
module HGeometry.Plane.LowerEnvelope.Type
  ( LowerEnvelope(..)
  , ParallelPlane(..)
  ) where

import           Control.Lens
import qualified Data.Set as Set
import           HGeometry.Plane.LowerEnvelope.Connected
import           HGeometry.Properties

--------------------------------------------------------------------------------
-- * Data type defining a lower envelope

-- | The lower enevelope of planes in R^3. (Or rather, its minimization diagram)
data LowerEnvelope plane =
    ParallelStrips    !(Set.Set (ParallelPlane plane))
  | ConnectedEnvelope !(MinimizationDiagram (NumType plane) plane)

deriving instance (Show plane, Show (NumType plane)) => Show (LowerEnvelope plane)
deriving instance (Eq plane, Eq (NumType plane))     => Eq (LowerEnvelope plane)

-- | Just a newtype around plane, to be used to model parallel strips in the Lower envelope.
newtype ParallelPlane plane = ParallelPlane plane
  deriving (Show,Eq)

instance Wrapped (ParallelPlane plane) where
  type Unwrapped (ParallelPlane plane) = plane
  _Wrapped' = coerced
