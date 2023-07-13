--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Polygon.Convex.MinkowskiSum
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Computes the Minkowski Sum of two convex polygons. Based on the
-- implementation in "Computational Geometry" by de Berg, van Kreveld,
-- Overmars, and Schwartskopf.
--
--------------------------------------------------------------------------------
module HGeometry.Polygon.Convex.MinkowskiSum
  ( minkowskiSum
  ) where

import Control.Lens
import Data.Default.Class
import Data.List.NonEmpty (NonEmpty(..))
import Data.Ord (comparing)
import HGeometry.Ext
import HGeometry.Point
import HGeometry.Polygon.Class
import HGeometry.Polygon.Convex.Class
import HGeometry.Polygon.Convex.Implementation
import HGeometry.Polygon.Simple.Class
import Hiraffe.Graph

--------------------------------------------------------------------------------


-- | Computes the Minkowski sum of the two input polygons with \(n\)
-- and \(m\) vertices respectively.
--
-- Note: it takes te position of te second polyon as is; you may want to translate it to
-- te oriin first.
  --
-- running time: \(O(n+m)\).
minkowskiSum     :: ( Ord r, Num r
                    , ConvexPolygon_ convexPolygon  point r
                    , ConvexPolygon_ convexPolygon' point' r
                    , Default point'
                    )
                 => convexPolygon -> convexPolygon'
                 -> ConvexPolygon (point :+ point')
minkowskiSum p q = uncheckedFromCCWPoints $ merge' (theVertices p) (theVertices q)
  where
    theVertices p' = case toNonEmptyOf (ccwOuterBoundaryFrom $ bottomMost p') p' of
                       v :| xs -> v:| (xs++[v])

    v .+. w = v .+^ (w^.vector) :+ w

    cmpAngle v v' w w' =
      ccwCmpAround origin (Point $ v' .-. v) (Point $ w' .-. w)

    merge' vs0@(v:|vs1) ws0@(w:|ws1) = go vs1 ws1
      where
        go []      []      = []
        go []      (w':ws) = v .+. w : merge' vs0      (w':|ws)
        go (v':vs) []      = v .+. w : merge' (v':|vs) ws0
        go (v':vs) (w':ws) = v .+. w : case cmpAngle v v' w w' of
          LT -> merge' (v':|vs) ws0
          GT -> merge' vs0      (w':|ws)
          EQ -> merge' (v':|vs) (w':|ws)

bottomMost :: (Polygon_ polygon point r, Ord r) => polygon -> VertexIx polygon
bottomMost = fst . first1Of (minimumVertexBy cmp . withIndex)
  where
    cmp p q = comparing (^.yCoord) p q <> comparing (^.xCoord) p q


-- newtype UnsafeExtra extra = UnsafeExtra extra

-- instance {-# OVERLAPS #-} (Point_ point d r) => Point_ (point :+ UnsafeExtra extra) d r where
--   fromVector v = fromVector v :+ undefined
