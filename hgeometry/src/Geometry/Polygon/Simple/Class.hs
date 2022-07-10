--------------------------------------------------------------------------------
-- |
-- Module      :  Geometry.Polygon.Simple.Class
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- A Simple polygon and some basic functions to interact with them.
--
--------------------------------------------------------------------------------
module Geometry.Polygon.Simple.Class
  ( SimplePolygon_(..)
  ) where

import           Control.Lens
import           Geometry.Polygon.Class
import           Geometry.Point.Class

--------------------------------------------------------------------------------

class ( HasOuterBoundary (polygon point r)
      , Vertex      (polygon point r) ~ point 2 r
      , Point_ point 2 r
      , VertexIx    (polygon point r) ~ Int
      ) => SimplePolygon_ polygon point r where

  -- | given the vertices of the polygon, in CCW order, constructs the
  -- polygon. The vertices are numbered in the order they are given.
  --
  -- pre: - the vertices are given in CCW order
  --      - at least 3 vertices, not all colinear
  --      - no repeated vertices
  --      - no self-inttersections
  uncheckedFromCCWPoints :: Foldable f => f (point 2 r) -> polygon point r

  -- | given the vertices of the polygon, constructs the polygon. The
  -- vertices are numbered in the order they are given.
  fromPoints :: Foldable f => f (point 2 r) -> Maybe (polygon point r)



-- class ( Polygon_ polygon point r ) => SimplePolygon_ polygon point r where


-- class Polygon_ polygon point r where
