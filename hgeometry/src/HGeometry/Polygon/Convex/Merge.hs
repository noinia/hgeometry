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

import           Control.Lens
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           HGeometry.Ext
import           HGeometry.LineSegment
import           HGeometry.Polygon.Class
import           HGeometry.Polygon.Convex.Class
import           HGeometry.Polygon.Convex.Tangents
import           HGeometry.Polygon.Simple.Class

--------------------------------------------------------------------------------

-- | Merging two convex hulls, based on the paper:
--
-- Two Algorithms for Constructing a Delaunay Triangulation
-- Lee and Schachter
-- International Journal of Computer and Information Sciences, Vol 9, No. 3, 1980
--
-- : (combined hull, lower tangent that was added, upper tangent that was
-- added)
--
-- pre: - lp and rp are disjoint, and there is a vertical line separating
--        the two polygons.
--
-- Running time: O(n+m), where n and m are the sizes of the two polygons respectively
--
-- FIXME: This currently returns the VertexIx's in the *original* polygons, rather than
-- in the new polygon.
merge       :: (ConvexPolygon_ convexPolygon point r, Num r, Ord r)
            => convexPolygon -> convexPolygon
            -> ( convexPolygon
               , ClosedLineSegment (point :+ VertexIx convexPolygon)
               , ClosedLineSegment (point :+ VertexIx convexPolygon)
               )
merge lp rp = (uncheckedFromCCWPoints $ r' <> l', lt, ut)
  where
    lt = lowerTangent lp rp
    ut = upperTangent lp rp
    r' = slice (lt^.end.extra)   (ut^.end.extra)   rp
    l' = slice (ut^.start.extra) (lt^.start.extra) lp

    slice i j pg = fmap snd . takeUntil ((== j) . fst) $ pg^..ccwOuterBoundaryFrom i.withIndex

-- | pre: predicate is true for at least one element.
--
-- >>> takeUntil (== 5) [1..10]
-- 1 :| [2,3,4,5]
-- >>> takeUntil even [1..10]
-- 1 :| [2]
takeUntil   :: (a -> Bool) -> [a] -> NonEmpty a
takeUntil p = NonEmpty.fromList . foldr (\x acc -> if p x then [x] else x:acc)  []
              -- precondition guarantees there is at least one elem in the output

-- takeUntil      :: (a -> Bool) -> [a] -> [a]
-- takeUntil p xs = let (xs',x:_) = break p xs in xs' <> [x]



-- rotateTo'   :: Eq a => (a :+ b) -> CircularVector (a :+ b) -> CircularVector (a :+ b)
-- rotateTo' x = fromJust . CV.findRotateTo (coreEq x)

-- coreEq :: Eq a => (a :+ b) -> (a :+ b) -> Bool
-- coreEq = (==) `on` (^.core)
