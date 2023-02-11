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
  , vector
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
import Vector

--------------------------------------------------------------------------------

-- | Traversal over the components of the vector
coordinates :: IndexedTraversal1' Int Point R
coordinates = reindexed (+1) $ vector .> components
{-# INLINE coordinates #-}

-- | Lens to access the i^th coordinate.
coord :: forall i. (KnownNat i, 1 <= i, (i-1) < D) => IndexedLens' Int Point R
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

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- * Convenience functions to construct 1..4 dimensional points

-- -- | A bidirectional pattern synonym for 1 dimensional points.
-- pattern Point1   :: R -> Point
-- pattern Point1 x = Point (Vector1 x)
-- {-# COMPLETE Point1 #-}

-- -- | A bidirectional pattern synonym for 2 dimensional points.
-- pattern Point2       :: R -> R -> Point
-- pattern Point2 x y = Point (Vector2 x y)
-- {-# COMPLETE Point2 #-}

-- -- | A bidirectional pattern synonym for 3 dimensional points.
-- pattern Point3       :: R -> R -> R -> Point
-- pattern Point3 x y z = (Point (Vector3 x y z))
-- {-# COMPLETE Point3 #-}

-- -- | A bidirectional pattern synonym for 4 dimensional points.
-- pattern Point4         :: R -> R -> R -> R -> Point
-- pattern Point4 x y z w = (Point (Vector4 x y z w))
-- {-# COMPLETE Point4 #-}
