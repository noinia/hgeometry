{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}
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
  ( Point_(..), pattern Point1_, pattern Point2_, pattern Point3_, pattern Point4_
  , Point(Point, toVec, Point1, Point2, Point3, Point4)
  , Arity
  , origin, vector
  , pointFromPoint, pointFromList

  , coord
  , xCoord, yCoord, zCoord, wCoord

  , projectPoint

  , CCW(CCW, CW, CoLinear), ccw, isCoLinear

  , ccwCmpAround
  , cwCmpAround
  , ccwCmpAroundWith
  , cwCmpAroundWith
  , sortAround
  , insertIntoCyclicOrder

  , StrictCCW(SCCW, SCW)
  , strictCcw


  , Quadrant(..), quadrantWith, quadrant, partitionIntoQuadrants

  , cmpByDistanceTo, cmpInDirection

  , squaredEuclideanDist, euclideanDist
  , HasSquaredEuclideanDistance(..)

  , HasPoints(..)
  ) where

import HGeometry.Point.Boxed
import HGeometry.Point.Class
import HGeometry.Point.EuclideanDistance
import HGeometry.Point.Orientation
import HGeometry.Point.Orientation.Degenerate
import HGeometry.Point.Quadrants
import HGeometry.HyperPlane.Class
import HGeometry.HyperPlane

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
cmpInDirection       :: ( Ord r, Num r, Point_ point d r, vector ~ Diff_ point
                        , MkHyperPlaneConstraints d
                        )
                     => vector -> point -> point -> Ordering
cmpInDirection n p q = p `onSideTest` fromPointAndNormal q n
