{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Geometry.Point
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- \(d\)-dimensional points.
--
--------------------------------------------------------------------------------
module Data.Geometry.Point( Point(.., Point1, Point2, Point3)
                          , origin, vector
                          , pointFromList
                          , projectPoint

                          , xCoord, yCoord, zCoord

                          , PointFunctor(..)

                          , CCW, ccw, ccw', isCoLinear
                          , pattern CCW, pattern CW, pattern CoLinear

                          , ccwCmpAround, ccwCmpAround'
                          , cwCmpAround, cwCmpAround'
                          , ccwCmpAroundWith, ccwCmpAroundWith'
                          , cwCmpAroundWith, cwCmpAroundWith'
                          , sortAround, sortAround'
                          , insertIntoCyclicOrder

                          , Quadrant(..), quadrantWith, quadrant, partitionIntoQuadrants

                          , cmpByDistanceTo, cmpByDistanceTo'

                          , squaredEuclideanDist, euclideanDist

                          , coord, unsafeCoord
                          ) where

import Data.Geometry.Point.Class
import Data.Geometry.Point.Internal hiding (coord, unsafeCoord)
import Data.Geometry.Point.Orientation.Degenerate
import Data.Geometry.Point.Quadrants
