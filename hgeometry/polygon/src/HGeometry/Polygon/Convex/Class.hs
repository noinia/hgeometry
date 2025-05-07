--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Polygon.Simple
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- A Simple polygon and some basic functions to interact with them.
--
--------------------------------------------------------------------------------
module HGeometry.Polygon.Convex.Class
  ( ConvexPolygon_ -- (..)
  ) where

import HGeometry.Polygon.Simple.Class
import HGeometry.Ext
import Data.Default.Class

--------------------------------------------------------------------------------

-- | Class modelling convex polygons.
class ( SimplePolygon_ convexPolygon point r
      ) => ConvexPolygon_ convexPolygon point r where

instance (ConvexPolygon_ convexPolygon point r, Default extra)
         => ConvexPolygon_ (convexPolygon :+ extra) point r where
