--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Polygon.Simple.Class
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- A Simple polygon and some basic functions to interact with them.
--
--------------------------------------------------------------------------------
module HGeometry.Polygon.Simple.Class
  ( SimplePolygon_(..)

  -- , signedArea, area2X
  ) where

import           Control.Lens
import           Data.Default.Class
import qualified Data.Foldable as F
import           Data.Foldable1
import           Data.Kind (Constraint)
import           HGeometry.Ext
import           HGeometry.Point.Class
import           HGeometry.Polygon.Class
import           HGeometry.Vector

--------------------------------------------------------------------------------

-- | A class representing simple polygons; i.e. polygons without holes
-- (and without self intersections in the boundary.)
class ( Polygon_ simplePolygon point r
      , VertexIx simplePolygon ~ Int
      , Point_ point 2 r
      ) => SimplePolygon_ simplePolygon point r where

  -- | Given the vertices of the polygon, in CCW order, constructs the
  -- polygon. The vertices are numbered in the order they are given.
  --
  -- pre: - the vertices are given in CCW order
  --      - at least 3 vertices, not all colinear
  --      - no repeated vertices
  --      - no self-inttersections
  uncheckedFromCCWPoints :: Foldable1 f => f point -> simplePolygon

  -- | Constraints that allow us to construct a simple polygon
  type ConstructableSimplePolygon simplePolygon point r :: Constraint
  type ConstructableSimplePolygon simplePolygon point r = (Eq r, Num r)

  -- | Given the vertices of the polygon, constructs the polygon. This
  -- function will make sure the polygon is a valid simple polygon,
  -- i.e. it has at least three vertices, is given in CCW order, no
  -- repeated vertices etc.
  --
  -- In particular, it will drop repeated vertices.
  fromPoints :: ( Foldable f
                , ConstructableSimplePolygon simplePolygon point r
                )
             => f point -> Maybe simplePolygon

  -- | Compute the centroid of a simple polygon.
  --
  -- running time: \(O(n)\)
  centroid      :: (Fractional r, ConstructablePoint_ point' 2 r) => simplePolygon -> point'
  centroid poly = fromVector $ sum' xs ^/ (3 * signedArea2X poly)
    where
      xs = [ (p^.vector ^+^ q^.vector) ^* (p^.xCoord * q^.yCoord - q^.xCoord * p^.yCoord)
           | (p,q) <- poly ^..outerBoundaryEdges   ]
      sum' = F.foldl' (^+^) zero

instance ( SimplePolygon_ simplePolygon point r
         , Default extra
         )
         => SimplePolygon_ (simplePolygon :+ extra) point r where
  uncheckedFromCCWPoints = (:+ def) . uncheckedFromCCWPoints
  type ConstructableSimplePolygon (simplePolygon :+ extra) point r =
         (ConstructableSimplePolygon simplePolygon point r, Default extra)
  fromPoints = fmap (:+ def) . fromPoints

--------------------------------------------------------------------------------


-- -- | Compute the signed area of a simple polygon. When the vertices
-- -- are given in counter clockwise order (as they should be), the area
-- -- will be positive.
-- signedArea      :: (Fractional r, SimplePolygon_ simplePolygon point r)
--                 => simplePolygon -> r
-- signedArea poly = signedArea2X poly / 2
