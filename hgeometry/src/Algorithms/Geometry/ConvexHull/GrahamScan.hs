--------------------------------------------------------------------------------
-- |
-- Module      :  Algorithms.Geometry.ConvexHull.GrahamScan
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--------------------------------------------------------------------------------
module Algorithms.Geometry.ConvexHull.GrahamScan( convexHull
                                                , upperHull, upperHull'
                                                , lowerHull, lowerHull'

                                                , upperHullFromSorted, upperHullFromSorted'
                                                ) where

import           Control.Lens ((^.))
import           Data.Ext
import           Geometry.Point
import           Geometry.Polygon
import           Geometry.Polygon.Convex (ConvexPolygon, mkConvexPolygon)
import qualified Data.List.NonEmpty as NonEmpty
import           Data.List.NonEmpty (NonEmpty(..))


-- | \(O(n \log n)\) time ConvexHull using Graham-Scan. The resulting polygon is
-- given in clockwise order.
convexHull            :: (Ord r, Num r, Point_ point 2 r
                        , AsExt (point 2 r)
                        , CoreOf (point 2 r) ~ Point 2 r
                        )
                      => NonEmpty (point 2 r) -> ConvexPolygon (ExtraOf (point 2 r)) r
convexHull (p :| []) = mkConvexPolygon [p]
convexHull ps        = let ps' = NonEmpty.toList . NonEmpty.sortBy incXdecY $ ps
                           uh  = NonEmpty.tail . hull' $         ps'
                           lh  = NonEmpty.tail . hull' $ reverse ps'
                       in mkConvexPolygon . reverse $ lh ++ uh

-- | Computes the upper hull. The upper hull is given from left to right.
--
-- Specifically. A pair of points defines an edge of the upper hull
-- iff all other points are strictly to the right of its supporting
-- line.
--
-- Note that this definition implies that the segment may be
-- vertical. Use 'upperHull'' if such an edge should not be reported.
--
-- running time: \(O(n\log n)\)
upperHull  :: (Ord r, Num r, Point_ point 2 r) => NonEmpty (point 2 r) -> NonEmpty (point 2 r)
upperHull = NonEmpty.reverse . hull id

-- | Computes the upper hull, making sure that there are no vertical segments.
--
-- The upper hull is given from left to right
--
upperHull'  :: (Ord r, Num r, Point_ point 2 r) => NonEmpty (point 2 r) -> NonEmpty (point 2 r)
upperHull' = NonEmpty.reverse . dropVertical . hull id

-- | Helper function to remove vertical segments from the hull.
--
-- Tests if the first two points are on a vertical line, if so removes
-- the first point.
dropVertical :: (Eq r, Point_ point 2 r) => NonEmpty (point 2 r) -> NonEmpty (point 2 r)
dropVertical = \case
  h@(_ :| [])                                  -> h
  h@(p :| (q : rest)) | p^.xCoord == q^.xCoord -> q :| rest
                      | otherwise              -> h


-- | Computes the upper hull. The upper hull is given from left to right.
--
-- Specifically. A pair of points defines an edge of the lower hull
-- iff all other points are strictly to the left of its supporting
-- line.
--
-- Note that this definition implies that the segment may be
-- vertical. Use 'lowerHull'' if such an edge should not be reported.
--
-- running time: \(O(n\log n)\)
lowerHull :: (Ord r, Num r, Point_ point 2 r) => NonEmpty (point 2 r) -> NonEmpty (point 2 r)
lowerHull = hull reverse

-- | Computes the lower hull, making sure there are no vertical
-- segments. (Note that the only such segment could be the first
-- segment).
lowerHull' :: (Ord r, Num r, Point_ point 2 r) => NonEmpty (point 2 r) -> NonEmpty (point 2 r)
lowerHull' = dropVertical . hull reverse

-- | Helper function so that that can compute both the upper or the lower hull, depending
-- on the function f
hull               :: (Ord r, Num r, Point_ point 2 r)
                   => ([point 2 r] -> [point 2 r])
                   -> NonEmpty (point 2 r) -> NonEmpty (point 2 r)
hull _ h@(_ :| []) = h
hull f pts         = hull' .  f
                   . NonEmpty.toList . NonEmpty.sortBy incXdecY $ pts

incXdecY  :: (Ord r, Point_ point 2 r) => point 2 r -> point 2 r -> Ordering
incXdecY (Point2_ px py) (Point2_ qx qy) =
  compare px qx <> compare qy py


-- | Given a sequence of points that is sorted on increasing
-- x-coordinate and decreasing y-coordinate, computes the upper
-- hull, in *right to left order*.
--
-- Specifically. A pair of points defines an edge of the upper hull
-- iff all other points are strictly to the right of its supporting
-- line.
--
--
-- Note that In constrast to the 'upperHull' function, the result is
-- returned *from right to left* !!!
--
-- running time: \(O(n)\).
upperHullFromSorted :: (Ord r, Num r, Point_ point 2 r) => NonEmpty (point 2 r) -> NonEmpty (point 2 r)
upperHullFromSorted = \case
  h@(_ :| [])  -> h
  pts          -> hull' $ NonEmpty.toList pts

-- | Computes the upper hull from a sorted input. Removes the last vertical segment.
--
--
-- running time: \(O(n)\).
upperHullFromSorted' :: (Ord r, Num r, Point_ point 2 r)
                     => NonEmpty (point 2 r) -> NonEmpty (point 2 r)
upperHullFromSorted' = dropVertical . upperHullFromSorted


-- | Precondition: The list of input points is sorted
hull'          :: (Ord r, Num r, Point_ point 2 r) => [point 2 r] -> NonEmpty (point 2 r)
hull' (a:b:ps) = NonEmpty.fromList $ hull'' [b,a] ps
  where
    hull'' h []      = h
    hull'' h (p:ps') = hull'' (cleanMiddle (p:h)) ps'

    cleanMiddle h@[_,_] = h
    cleanMiddle h@(z:y:x:rest)
      | rightTurn x y z = h
      | otherwise       = cleanMiddle (z:x:rest)
    cleanMiddle _       = error "cleanMiddle: too few points"
hull' _ = error
  "Algorithms.Geometry.ConvexHull.GrahamScan.hull' requires a list with at least \
  \two elements."

rightTurn       :: (Ord r, Num r, Point_ point 2 r) => point 2 r -> point 2 r -> point 2 r -> Bool
rightTurn a b c = ccw a b c == CW
