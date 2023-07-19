--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Polygon.Convex
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- A type for representing Convex polygons, and some basic functions
-- on Convex polygons.
--
--------------------------------------------------------------------------------
module HGeometry.Polygon.Convex
  ( ConvexPolygon_ -- (..)
  , ConvexPolygon
  , ConvexPolygonF
  , fromSimplePolygon, toSimplePolygon
  , _ConvexPolygon
  , isStrictlyConvex, isConvex
  , verifyConvex
  , minkowskiSum
  , maxInDirection
  ) where

import HGeometry.Polygon.Convex.Class
import HGeometry.Polygon.Convex.Implementation
import HGeometry.Polygon.Convex.MinkowskiSum
