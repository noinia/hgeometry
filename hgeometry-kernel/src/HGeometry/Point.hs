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
  , Point_(..), pattern Point1_, pattern Point2_, pattern Point3_, pattern Point4_
  , PointF(Point, toVec)
  , origin, vector
  , pointFromPoint, pointFromList

  , coord
  , xCoord, yCoord, zCoord, wCoord

  , projectPoint

--  , CCW(CCW, CW, CoLinear), ccw, isCoLinear
--
--  , ccwCmpAround
--  , cwCmpAround
--  , ccwCmpAroundWith
--  , cwCmpAroundWith
--  , sortAround
--  , insertIntoCyclicOrder
--
--  , StrictCCW(SCCW, SCW)
--  , strictCcw
--
--
  , Quadrant(..), quadrantWith, quadrant, partitionIntoQuadrants

  , cmpByDistanceTo, cmpInDirection

  , squaredEuclideanDist, euclideanDist
  , HasSquaredEuclideanDistance(..)

  , HasPoints(..)
  , PointFor
  ) where

import Control.Lens ((^.))
import Data.Type.Ord
import GHC.TypeLits
import HGeometry.HyperPlane
import HGeometry.Point.Class
import HGeometry.Point.EuclideanDistance
-- import HGeometry.Point.Orientation
-- import HGeometry.Point.Orientation.Degenerate
import HGeometry.Point.Quadrants
import HGeometry.Vector.Class
import HGeometry.Vector.Optimal (VectorFamily')
import HGeometry.Point.Optimal

--------------------------------------------------------------------------------

-- $setup
-- >>> import HGeometry.Vector


-- | Compare the points with respect to the direction given by the
-- vector, i.e. by taking planes whose normal is the given vector.
--
-- >>> cmpInDirection (Vector2 1 0) (Point2 5 0) (Point2 10 0)
-- LT
-- >>> cmpInDirection (Vector2 1 1) (Point2 5 0) (Point2 10 0)
-- LT
-- >>> cmpInDirection (Vector2 1 1) (Point2 5 0) (Point2 10 10)
-- LT
-- >>> cmpInDirection (Vector2 1 1) (Point2 15 15) (Point2 10 10)
-- GT
-- >>> cmpInDirection (Vector2 1 0) (Point2 15 15) (Point2 15 10)
-- EQ
cmpInDirection       :: forall vector point d r.
                        ( Ord r, Num r, Point_ point d r
                        , vector ~ VectorFor point
                        , d < d+1, d <= d+1, 0 < d+1, 0 < d
                        , KnownNat ((d+1)-d), KnownNat d
                        , Vector_ (VectorFamily' d r) d r
                        , Vector_ (VectorFamily' (d+1) r) (d+1) r
                        )
                     => vector -> point -> point -> Ordering
cmpInDirection n p q = p `onSideTest` fromPointAndNormal' q n
  where
    fromPointAndNormal' q' n' = HyperPlane $ cons a0 n'
      where
        a0 = negate $ (q'^.vector) `dot` n'
