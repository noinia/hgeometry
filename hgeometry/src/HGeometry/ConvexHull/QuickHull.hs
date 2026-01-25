--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.ConvexHull.QuickHull
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--------------------------------------------------------------------------------
module HGeometry.ConvexHull.QuickHull
  ( convexHull
  ) where

import           Control.Lens
import qualified Data.Foldable as F
import qualified Data.List as List
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Ord (comparing)
import           HGeometry.Line
import           HGeometry.Line.PointAndVector(liesAbove)
import           HGeometry.Point
import           HGeometry.Polygon.Convex
import           HGeometry.Polygon.Simple
import           HGeometry.Triangle
import           HGeometry.Intersection

--------------------------------------------------------------------------------

-- | ConvexHull using Quickhull.
--
-- pre: input contains at least three points
--
-- running time: \(O(n^2)\)
convexHull            :: (Ord r, Fractional r, Show r, Point_ point 2 r)
                      => NonEmpty point -> ConvexPolygon point
convexHull ps        = uncheckedFromCCWPoints
                     $ l :| hull l r below' <> [r] <> reverse (hull l r above)
  where
    Extremes l r mids = findExtremes ps
    m                 = lineThrough l r
    (above,below')    = List.partition (`liesAbove` m) mids

data Extremes p = Extremes !p !p [p] deriving (Show)

-- | Finds the leftmost and rightmost point in the list
findExtremes            :: (Ord r, Point_ point 2 r)
                        => NonEmpty point
                        -> Extremes point
findExtremes (p :| pts ) = foldr f (Extremes p p []) pts
  where
    f q (Extremes l r ms) = case (incXdecY q l, incXdecY q r) of
                              (LT,_)  -> Extremes q r (addIfNot r l ms)
                              (EQ,_)  -> Extremes l r ms -- ditch q; it is the same as l
                              (GT,GT) -> Extremes l q (addIfNot l r ms)
                              (GT,EQ) -> Extremes l r ms -- ditch q; it is the same as r
                              (GT,LT) -> Extremes l r (q:ms)

    addIfNot y x xs | (x^.asPoint) /= (y^.asPoint) = x:xs
                    | otherwise                    = xs

-- findExtremesBy         :: (a -> a -> Ordering)
--                        -> NonEmpty a
--                        -> STR a a [a]
-- findExtremesBy cmp pts = let l = F.minimumBy cmp pts
--                              r = F.maximumBy cmp pts
--                              a /=. b = a `cmp` b /= EQ
--                          in STR l r [p | p <- F.toList pts, p /=. l, p /=. r]


incXdecY :: (Ord r, Point_ point 2 r) => point -> point -> Ordering
incXdecY (Point2_ px py) (Point2_ qx qy) =
  compare px qx <> compare qy py

-- | include neigher left or right
--
hull         :: forall point r. (Fractional r, Ord r, Point_ point 2 r)
             => point -> point -> [point ] -> [point]
hull _ _ []  = []
hull l r pts = hull l mid ls <> [mid] <> hull mid r rs
  where
    m       :: LinePV 2 r
    m       = lineThrough l r
    mid     = F.maximumBy (comparing dist) pts

    dist p = p `squaredEuclideanDistTo` m
    t       = Triangle l mid r
    -- line through l and mid, which splits the remaining points in a left half and a right half.
    splitL   = lineThrough l mid
    rightSide = r `onSide` splitL -- define the side containing r the right side

    (ls,rs) = List.partition (\p -> p `onSide` splitL /= rightSide)
            . filter (\p -> not $ (p^.asPoint) `intersects` t) $ pts
