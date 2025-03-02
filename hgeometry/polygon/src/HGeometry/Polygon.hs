--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Polygon
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- A polygon and some basic functions to interact with them.
--
--------------------------------------------------------------------------------
module HGeometry.Polygon
  ( module HGeometry.Polygon.Class
  , asTriangle
  , area2X
  , area

  , module HGeometry.Polygon.Simple
  , module HGeometry.Polygon.Convex
  ) where

import Control.Lens hiding (holes)
import HGeometry.Point.Class
import HGeometry.Polygon.Class
import HGeometry.Polygon.Simple
import HGeometry.Polygon.Convex
import HGeometry.Triangle

--------------------------------------------------------------------------------

-- | Try to convert the polygon into a triangle (whose vertices are given in CCW order).
asTriangle    :: Polygon_ polygon point r => polygon -> Maybe (Triangle point)
asTriangle pg = case pg^..vertices of
                  [u,v,w] -> Just $ Triangle u v w
                  _       -> Nothing

-- | The area of a polygon
--
-- running time: \(O(n)\)
area :: ( Polygon_ polygon point r
        , SimplePolygon_ (Hole polygon) point r
        , Fractional r) => polygon -> r
area = (/2) . area2X

-- | Computes the double area of a polygon
area2X      :: ( Polygon_ polygon point r
               , Num r
               , Point_ point 2 r
               , SimplePolygon_ (Hole polygon) point r
               ) => polygon -> r
area2X poly = signedArea2X poly - sumOf (holes.to signedArea2X) poly
