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

  , signedArea, area2X
  ) where

import           Control.Lens
import           Geometry.Polygon.Class
import           Geometry.Point.Class
import           Geometry.Vector
import qualified Data.Foldable as F

--------------------------------------------------------------------------------

-- | A class representing simple polygons; i.e. polygons without holes
-- (and without self intersections in the boundary.)
class ( Polygon_ simplePolygon point r
      , VertexIx (simplePolygon point r) ~ Int
      ) => SimplePolygon_ simplePolygon point r where

  -- | given the vertices of the polygon, in CCW order, constructs the
  -- polygon. The vertices are numbered in the order they are given.
  --
  -- pre: - the vertices are given in CCW order
  --      - at least 3 vertices, not all colinear
  --      - no repeated vertices
  --      - no self-inttersections
  uncheckedFromCCWPoints :: Foldable f => f (point 2 r) -> simplePolygon point r

  -- | given the vertices of the polygon, constructs the polygon. The
  -- vertices are numbered in the order they are given.
  fromPoints :: Foldable f => f (point 2 r) -> Maybe (simplePolygon point r)

  -- | Compute the centroid of a simple polygon.
  centroid      :: Fractional r => simplePolygon point r -> point 2 r
  centroid poly = fromVector $ sum' xs ^/ (6 * signedArea poly)
    where
      xs = [ (p^.asVector ^+^ q^.asVector) ^* (p^.xCoord * q^.yCoord - q^.xCoord * p^.yCoord)
           | (p,q) <- poly ^..outerBoundaryEdges   ]
      sum' = F.foldl' (^+^) zero


--------------------------------------------------------------------------------

-- | Compute the signed area of a simple polygon. When the vertices
-- are given in counter clockwise order (as they should be), the area
-- will be positive.
signedArea      :: (Fractional r, SimplePolygon_ simplePolygon point r)
                => simplePolygon point r -> r
signedArea poly = signedArea2X poly / 2

-- | Compute the area times 2 of a simple polygon. When the vertices
-- are given in counter clockwise order (as they should be), the area
-- will be positive.
--
-- If the vertices were in clockwise order, the signed area would have
-- be negative.
--
-- running time: \(O(n)\)
area2X :: (Num r, SimplePolygon_ simplePolygon point r)
       => simplePolygon point r -> r
area2X = signedArea2X




-- | \( O(n) \) Test if the outer boundary of the polygon is in clockwise or counter
-- clockwise order.
-- isCounterClockwise :: (Eq r, Num r) => Polygon t p r -> Bool
-- isCounterClockwise = (\x -> x == abs x) . signedArea2X . view outerBoundary
