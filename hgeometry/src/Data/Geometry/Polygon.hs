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
                            , connectHoles

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
import           Control.Lens                                  hiding (Simple)
import           Control.Monad.Random.Class
import           Data.Ext
import qualified Data.Foldable                                 as F
import           Data.Geometry.HalfSpace                       (rightOf)
import           Data.Geometry.Line
import           Data.Geometry.Point
import           Data.Geometry.Polygon.Core
import           Data.Geometry.Polygon.Extremes
import           Data.Geometry.Vector
import           Data.List


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

connectHoles :: (Ord r, Num r) => MultiPolygon p r -> SimplePolygon p r
connectHoles = connectHolesLinear (Vector2 0 1)

-- | Connect holes with the outer hull by linear cuts.
--
-- \(O(n)\)
connectHolesLinear :: (Ord r, Num r) => Vector 2 r -> MultiPolygon p r -> SimplePolygon p r
connectHolesLinear vec (MultiPolygon border holes) =
    connectHolesLinear' vec (SimplePolygon border) sorted
  where
    extremes = map (snd . extremesLinear vec) holes
    sorted = sortBy (\a b -> cmpExtreme vec (snd a) (snd b)) (zip holes extremes)

connectHolesLinear' :: Vector 2 r -> SimplePolygon p r
                    -> [(SimplePolygon p r, Point 2 r :+ p)] -> SimplePolygon p r
connectHolesLinear' _ border []      = border
connectHolesLinear' vec border ((hole, extreme):holes) =
  --
  undefined
  where
    worker acc [] = []
    -- worker acc (l:ls)
    --   | line intersects = mkPoly (reverse acc ++ lBegin ++ lEnd ++ ls) intersect_point : worker (l:acc) ls

-- for each hole, find extreme point
-- pick hole with largest extreme
-- cut from that hole to outer poly
-- iterate until no more holes

cutPolygon :: SimplePolygon () r -> Line 2 r -> [(Point 2 r, Seq (Point 2 r))]
cutPolygon poly line = worker [] (listEdges poly)
  where
    worker acc [] = []
    worker acc (segment:segments) =
      let acc' = segment : acc
          LineSegment begin end = segment
      in match (intersect segment line) $
             (H $ const $ worker acc' segments)
          :& (H $ \pt ->
              let firstSeg = LineSegment begin (Open pt)
                  secondSeg = LineSegment (Closed pt) end
              in (pt, secondSeg : segments ++ reverse acc ++ [secondSeg]) :
                  worker acc' segments)
          :& (H SegmentIntersection)
          :& RNil
