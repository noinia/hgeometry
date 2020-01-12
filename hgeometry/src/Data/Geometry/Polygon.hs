{-# LANGUAGE TemplateHaskell #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Geometry.Polygon
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- A Polygon data type and some basic functions to interact with them.
--
--------------------------------------------------------------------------------
module Data.Geometry.Polygon( PolygonType(..)
                            , Polygon(..)
                            , _SimplePolygon, _MultiPolygon
                            , SimplePolygon, MultiPolygon, SomePolygon

                            , fromPoints

                            , polygonVertices, listEdges

                            , outerBoundary, outerBoundaryEdges
                            , outerVertex, outerBoundaryEdge

                            , polygonHoles, polygonHoles'
                            , holeList

                            , inPolygon, insidePolygon, onBoundary

                            , area, signedArea

                            , centroid
                            , pickPoint

                            , isTriangle, isStarShaped

                            , isCounterClockwise
                            , toCounterClockWiseOrder, toCounterClockWiseOrder'
                            , toClockwiseOrder, toClockwiseOrder'
                            , reverseOuterBoundary

                            , findDiagonal

                            , withIncidentEdges, numberVertices

                            , asSimplePolygon
                            , extremesLinear, cmpExtreme
                            ) where

import           Algorithms.Geometry.LinearProgramming.LP2DRIC
import           Algorithms.Geometry.LinearProgramming.Types
import           Control.Lens hiding (Simple)
import           Control.Monad.Random.Class
import           Data.Ext
import qualified Data.Foldable as F
import           Data.Geometry.HalfSpace (rightOf)
import           Data.Geometry.Line
import           Data.Geometry.Point
import           Data.Geometry.Polygon.Core
import           Data.Geometry.Polygon.Extremes


--------------------------------------------------------------------------------
-- * Polygons

-- | Test if a Simple polygon is star-shaped. Returns a point in the kernel
-- (i.e. from which the entire polygon is visible), if it exists.
--
--
-- \(O(n)\) expected time
isStarShaped    :: (MonadRandom m, Ord r, Fractional r)
                => SimplePolygon p r -> m (Maybe (Point 2 r))
isStarShaped (toClockwiseOrder -> pg) =
    solveBoundedLinearProgram $ LinearProgram c (F.toList hs)
  where
    c  = pg^.outerVertex 1.core.vector
    -- the first vertex is the intersection point of the two supporting lines
    -- bounding it, so the first two edges bound the shape in this sirection
    hs = fmap (rightOf . supportingLine) . outerBoundaryEdges $ pg
