{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Point
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- \(d\)-dimensional points.
--
--------------------------------------------------------------------------------
module HGeometry.Point
  ( Point
  , PointF(Point, toVec, Point1, Point2, Point3, Point4)
  -- , {- | construct a 1-dimensional point -} pattern Point1
  -- , {- | construct a 2-dimensional point -} pattern Point2
  -- , {- | construct a 3-dimensional point -} pattern Point3
  -- , {- | construct a 4-dimensional point -} pattern Point4
  , Point_(..), pattern Point1_, pattern Point2_, pattern Point3_, pattern Point4_
  , ConstructablePoint_(..)
  , HasVector(..)
  , HasCoordinates(..)
  , asPoint
  , origin
  -- , pointFromPoint
  , pointFromList

  , coord
  , xCoord, yCoord, zCoord, wCoord

  , projectPoint

  , Affine_(..)
  , CCW(CCW, CW, CoLinear), ccw, isCoLinear

  , ccwCmpAround
  , cwCmpAround
  , ccwCmpAroundWith
  , cwCmpAroundWith
  , sortAround
  , insertIntoCyclicOrder
--
--  , StrictCCW(SCCW, SCW)
--  , strictCcw
--
--
 , Quadrant(..), quadrantWith, quadrant, partitionIntoQuadrants

  , cmpByDistanceTo
  , cmpInDirection

  , squaredEuclideanDist, euclideanDist
  , HasSquaredEuclideanDistance(..)

  , HasPoints(..), HasPoints'
  ) where

import Control.Lens (Lens', Iso', coerced, (^.))
import HGeometry.Point.Class
import HGeometry.Point.EuclideanDistance
import Data.Type.Ord
import HGeometry.Point.Orientation
import HGeometry.Point.Quadrants
import HGeometry.Vector
import HGeometry.Point.PointF
-- import Data.Coerce

--------------------------------------------------------------------------------
-- $setup
-- >>> import HGeometry.Vector
-- >>> import Control.Lens(Lens', Iso', (^.), coerced)

--------------------------------------------------------------------------------

-- | d-dimensional points
type Point d r = PointF (Vector d r)

-- | Convert a generic point into a Point d r, dropping any additional
-- information we may now about it.
asPoint :: forall point d r. Point_ point d r => Lens' point (Point d r)
asPoint = vector.(coerced :: Iso' (Vector d r) (Point d r))
{-# INLINE asPoint #-}

--------------------------------------------------------------------------------
-- * Convenience functions to construct 1, 2 and 3 dimensional points

-- | A bidirectional pattern synonym for 1 dimensional points.
pattern Point1   :: r -> Point 1 r
pattern Point1 x = Point (Vector1 x)
{-# COMPLETE Point1 #-}

-- | A bidirectional pattern synonym for 2 dimensional points.
pattern Point2       :: r -> r -> Point 2 r
pattern Point2 x y = Point (Vector2 x y)
{-# COMPLETE Point2 #-}

-- | A bidirectional pattern synonym for 3 dimensional points.
pattern Point3       :: r -> r -> r -> Point 3 r
pattern Point3 x y z = (Point (Vector3 x y z))
{-# COMPLETE Point3 #-}

-- | A bidirectional pattern synonym for 4 dimensional points.
pattern Point4         :: r -> r -> r -> r -> Point 4 r
pattern Point4 x y z w = (Point (Vector4 x y z w))
{-# COMPLETE Point4 #-}

--------------------------------------------------------------------------------

-- | Project a point into a lower dimension.
projectPoint   :: forall i point d r.
                  (Point_ point d r, i <= d, Has_ Vector_ i r) => point -> Point i r
projectPoint p = Point . prefix $ p^.asPoint.vector
