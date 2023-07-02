module HGeometry.Polygon.Convex
  ( ConvexPolygon_(..)
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
