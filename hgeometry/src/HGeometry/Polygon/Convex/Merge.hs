--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Polygon.Convex.Merge
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Algorithm for merging two disjoint convex polygons; i.e. computing their tangents and
-- combining them.
--
--------------------------------------------------------------------------------
module HGeometry.Polygon.Convex.Merge
  ( merge
  ) where

import HGeometry.Polygon.Convex.Class
import HGeometry.Polygon.Convex.Internal
import HGeometry.Polygon.Convex.Tangents

--------------------------------------------------------------------------------

-- | Rotating Right <-> rotate clockwise
--
-- Merging two convex hulls, based on the paper:
--
-- Two Algorithms for Constructing a Delaunay Triangulation
-- Lee and Schachter
-- International Journal of Computer and Information Sciences, Vol 9, No. 3, 1980
--
-- : (combined hull, lower tangent that was added, upper tangent thtat was
-- added)
--
-- pre: - lp and rp are disjoint, and there is a vertical line separating
--        the two polygons.
--      - The vertices of the polygons are given in clockwise order
--
-- Running time: O(n+m), where n and m are the sizes of the two polygons respectively
merge       :: (ConvexPolygon_ convexPolygon point r, Num r, Ord r)
            => convexPolygon -> convexPolygon
            -> (convexPolygon, LineSegment point, LineSegment point)
merge lp rp = (uncheckedFromCCWPoints $ r' <> l', lt, ut)
  where
    lt@(ClosedLineSegment a b) = lowerTangent lp rp
    ut@(ClosedLineSegment c d) = upperTangent lp rp

    takeUntil p xs = let (xs',x:_) = break p xs in xs' ++ [x]
    rightElems  = F.toList . CV.rightElements
    takeAndRotate x y = takeUntil (coreEq x) . rightElems . rotateTo' y . getVertices

    r' = takeAndRotate b d rp
    l' = takeAndRotate c a lp


rotateTo'   :: Eq a => (a :+ b) -> CircularVector (a :+ b) -> CircularVector (a :+ b)
rotateTo' x = fromJust . CV.findRotateTo (coreEq x)

coreEq :: Eq a => (a :+ b) -> (a :+ b) -> Bool
coreEq = (==) `on` (^.core)
