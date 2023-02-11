{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
module HGeometry.Point.Common
  ( coordinates
  , coord
  ) where


import Control.Lens
import Data.Type.Ord
import GHC.TypeLits
import D
import HGeometry.Sigs.Point
import R
import HGeometry.Sigs.Vector

--------------------------------------------------------------------------------

-- | Traversal over the components of the vector
coordinates :: IndexedTraversal1' Int Point R
coordinates = reindexed (+1) $ vector .> components
{-# INLINE coordinates #-}

-- | Lens to access the i^th coordinate.
coord :: forall i. (1 <= i, (i-1) < D) => IndexedLens' Int Point R
coord = reindexed (+1) $ vector .> component @(i-1)
{-# INLINE coord #-}

-- -- | Get the x-coordinate of ap oint
-- xCoord :: 1 <= D => IndexedLens' Int Point R
-- xCoord = coord @1
