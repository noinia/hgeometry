--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Point
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- d-Dimensional points
--
--------------------------------------------------------------------------------
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
module HGeometry.Point
  ( Point(..)
  , coordinates
  , coord
  , xCoord
  , yCoord
  , zCoord
  , wCoord
  , module HGeometry.Point.Affine
  ) where


import Control.Lens
import D
import Data.Type.Ord
import GHC.TypeLits
import HGeometry.Point.Affine
import HGeometry.Vector
import Point
import R

--------------------------------------------------------------------------------

-- | Traversal over the components of the vector
coordinates :: IndexedTraversal1' Int Point R
coordinates = reindexed (+1) $ vector .> components
{-# INLINE coordinates #-}

-- | Lens to access the i^th coordinate.
coord :: forall i. (1 <= i, (i-1) < D) => IndexedLens' Int Point R
coord = reindexed (+1) $ vector .> component @(i-1)
{-# INLINE coord #-}

-- | Get the x-coordinate of ap oint
xCoord :: 0 < D => IndexedLens' Int Point R
xCoord = coord @1
{-# INLINE xCoord #-}

-- | Get the x-coordinate of ap oint
yCoord :: 1 < D => IndexedLens' Int Point R
yCoord = coord @2
{-# INLINE yCoord #-}

-- | Get the x-coordinate of ap oint
zCoord :: 2 < D => IndexedLens' Int Point R
zCoord = coord @3
{-# INLINE zCoord #-}

-- | Get the x-coordinate of ap oint
wCoord :: 3 < D => IndexedLens' Int Point R
wCoord = coord @4
{-# INLINE wCoord #-}
