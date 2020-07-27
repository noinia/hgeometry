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
module Data.Geometry.Point( Point(..)
                          , origin, vector
                          , pointFromList
                          , projectPoint

                          , pattern Point1
                          , pattern Point2
                          , pattern Point3
                          , xCoord, yCoord, zCoord

                          , PointFunctor(..)

                          , CCW, ccw, ccw'
                          , pattern CCW, pattern CW, pattern CoLinear

                          , ccwCmpAround, cwCmpAround, ccwCmpAroundWith, cwCmpAroundWith
                          , sortAround, insertIntoCyclicOrder

                          , Quadrant(..), quadrantWith, quadrant, partitionIntoQuadrants

                          , cmpByDistanceTo

                          , squaredEuclideanDist, euclideanDist


                          , AsAPoint(..), coord, unsafeCoord, vector'
                          ) where

import Data.Geometry.Point.Class
import Data.Geometry.Point.Internal hiding (coord, unsafeCoord)
import Data.Geometry.Point.Orientation.Degenerate
import Data.Geometry.Point.Quadrants
