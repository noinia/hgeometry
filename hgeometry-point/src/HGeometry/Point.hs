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
  , Point_(..) -- , pattern Point1_, pattern Point2_, pattern Point3_, pattern Point4_
  , asPoint
  , origin, vector
  -- , pointFromPoint
  , pointFromList

  , coord
  , xCoord, yCoord, zCoord, wCoord

  -- , projectPoint

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
--  , Quadrant(..), quadrantWith, quadrant, partitionIntoQuadrants

  , cmpByDistanceTo
  -- , cmpInDirection

  , squaredEuclideanDist, euclideanDist
  , HasSquaredEuclideanDistance(..)

  , HasPoints(..), HasPoints'
  ) where

import Control.Lens (Lens', Iso', (^.), coerced)
import Data.Type.Ord
import GHC.TypeLits
-- import HGeometry.HyperPlane.Class
-- import HGeometry.HyperPlane.Internal
import HGeometry.Point.Class
import HGeometry.Point.EuclideanDistance
-- import HGeometry.Point.Orientation
import HGeometry.Point.Orientation.Degenerate
-- import HGeometry.Point.Quadrants
import HGeometry.Vector
import HGeometry.Point.PointF
import Data.Coerce
import HGeometry.Properties (NumType)

--------------------------------------------------------------------------------
-- $setup
-- >>> import HGeometry.Vector

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

-- | Compare the points with respect to the direction given by the
-- vector, i.e. by taking planes whose normal is the given vector.
--
-- >>> cmpInDirection (Vector2 1 0) (Point2 5 0) (Point2 10 (0 :: Int))
-- LT
-- >>> cmpInDirection (Vector2 1 1) (Point2 5 0) (Point2 10 (0 :: Int))
-- LT
-- >>> cmpInDirection (Vector2 1 1) (Point2 5 0) (Point2 10 (10 :: Int))
-- LT
-- >>> cmpInDirection (Vector2 1 1) (Point2 15 15) (Point2 10 (10 :: Int))
-- GT
-- >>> cmpInDirection (Vector2 1 0) (Point2 15 15) (Point2 15 (10 :: Int))
-- EQ
-- cmpInDirection       :: forall vector point d r.
--                         ( Ord r, Num r, Point_ point d r
--                         , vector ~ VectorFor point
--                         , d < d+1, 0 < d
--                         , KnownNat ((d+1)-d), KnownNat d
--                         , Vector_ (VectorFamily' d r) d r
--                         , Vector_ (VectorFamily' (d+1) r) (d+1) r
--                         )
--                      => vector -> point -> point -> Ordering
-- cmpInDirection n p q = p `onSideTest` fromPointAndNormal' q n
--   where
--     fromPointAndNormal' q' n' = HyperPlane $ cons a0 n'
--       where
--         a0 = negate $ (q'^.vector) `dot` n'
