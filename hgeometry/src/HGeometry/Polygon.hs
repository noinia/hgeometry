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
  ) where

import Control.Lens
import HGeometry.Polygon.Class
import HGeometry.Triangle

--------------------------------------------------------------------------------

-- | Try to convert the polygon into a triangle (whose vertices are given in CCW order).
asTriangle    :: Polygon_ polygon point r => polygon -> Maybe (Triangle point)
asTriangle pg = case pg^..vertices of
                  [u,v,w] -> Just $ Triangle u v w
                  _       -> Nothing
