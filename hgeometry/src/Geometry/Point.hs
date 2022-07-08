{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Geometry.Point
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- \(d\)-dimensional points.
--
--------------------------------------------------------------------------------
module Geometry.Point
  ( Point_(..)
  , Point(.., Point1, Point2, Point3, Point4)
  , origin, vector
  , pointFromList
  , projectPoint
  , coord, unsafeCoord
  , xCoord, yCoord, zCoord

  , PointFunctor(..)

  , CCW, ccw, isCoLinear
  , pattern CCW, pattern CW, pattern CoLinear

  , ccwCmpAround
  , cwCmpAround
  , ccwCmpAroundWith
  , cwCmpAroundWith
  , sortAround
  , insertIntoCyclicOrder

  , StrictCCW, pattern SCCW, pattern SCW
  , strictCcw


  , Quadrant(..), quadrantWith, quadrant, partitionIntoQuadrants

  , cmpByDistanceTo, cmpInDirection

  , squaredEuclideanDist, euclideanDist
  , HasSquaredEuclideanDistance(..)

  ) where

import Geometry.Line.Internal
import Geometry.Point.Class
import Geometry.Point.Boxed
import Geometry.Point.EuclideanDistance
import Geometry.Point.Orientation
import Geometry.Point.Orientation.Degenerate
import Geometry.Point.Quadrants
import Geometry.Vector

--------------------------------------------------------------------------------

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
cmpInDirection       :: (Ord r, Num r, Point_ point 2 r)
                     => Vector 2 r -> point 2 r -> point 2 r -> Ordering
cmpInDirection n p q = case p `onSide` perpendicularTo (Line q n) of
                         LeftSide  -> LT
                         OnLine    -> EQ
                         RightSide -> GT
  -- TODO: Generalize to arbitrary dimension
