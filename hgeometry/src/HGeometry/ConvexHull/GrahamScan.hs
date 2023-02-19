  -- {-# OPTIONS_GHC -ddump-simpl -dsuppress-module-prefixes -dsuppress-uniques  #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.ConvexHull.GrahamScan
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--------------------------------------------------------------------------------
module HGeometry.ConvexHull.GrahamScan
  ( convexHull
  , upperHull, upperHull'
  , lowerHull, lowerHull'

  , upperHullFromSorted, upperHullFromSorted'
  ) where

import           Control.Lens ((^.))
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Vector as Vector
import           HGeometry.Foldable.Sort
import           HGeometry.Point
import           HGeometry.Polygon.Convex
import           HGeometry.Polygon.Simple.Class
import           R

--------------------------------------------------------------------------------

-- | \(O(n \log n)\) time ConvexHull using Graham-Scan. The resulting polygon is
-- given in clockwise order.
--
-- pre: input contains at least three points
convexHull            :: Point_ point 2 R
                      => NonEmpty point -> ConvexPolygon point
-- convexHull (p :| []) = ConvexPolygon . unsafeFromPoints $ [p]
convexHull ps        = let ps' = Vector.toList . sortBy incXdecY $ ps
                           uh  = NonEmpty.tail . hull' $         ps'
                           lh  = NonEmpty.tail . hull' $ reverse ps'
                       in uncheckedFromCCWPoints . reverse $ lh ++ uh
-- {-# INLINABLE convexHull #-}

-- {-# SPECIALIZE convexHull :: NonEmpty (Point 2 Int) -> ConvexPolygon (Point 2 Int) #-}

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
upperHull  :: Point_ point 2 R => NonEmpty point -> NonEmpty point
upperHull = NonEmpty.reverse . hull id
-- {-# INLINABLE upperHull#-}

-- | Computes the upper hull, making sure that there are no vertical segments.
--
-- The upper hull is given from left to right
--
upperHull'  :: Point_ point 2 R => NonEmpty point -> NonEmpty point
upperHull' = NonEmpty.reverse . dropVertical . hull id
--{-# INLINABLE upperHull' #-}

-- | Helper function to remove vertical segments from the hull.
--
-- Tests if the first two points are on a vertical line, if so removes
-- the first point.
dropVertical :: (Eq r, Point_ point 2 r) => NonEmpty point -> NonEmpty point
dropVertical = \case
  h@(_ :| [])                                  -> h
  h@(p :| (q : rest)) | p^.xCoord == q^.xCoord -> q :| rest
                      | otherwise              -> h
--{-# INLINABLE dropVertical #-}

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
lowerHull :: Point_ point 2 R => NonEmpty point -> NonEmpty point
lowerHull = hull reverse
--{-# INLINABLE lowerHull #-}

-- | Computes the lower hull, making sure there are no vertical
-- segments. (Note that the only such segment could be the first
-- segment).
lowerHull' :: Point_ point 2 R => NonEmpty point -> NonEmpty point
lowerHull' = dropVertical . hull reverse
--{-# INLINABLE lowerHull' #-}

-- | Helper function so that that can compute both the upper or the lower hull, depending
-- on the function f
hull               :: Point_ point 2 R
                   => ([point] -> [point])
                   -> NonEmpty point -> NonEmpty point
hull _ h@(_ :| []) = h
hull f pts         = hull' .  f
                   . Vector.toList . sortBy incXdecY $ pts
--{-# INLINABLE hull #-}

incXdecY :: (Ord r, Point_ point 2 r) => point -> point -> Ordering
incXdecY (Point2_ px py) (Point2_ qx qy) =
  compare px qx <> compare qy py
--{-# INLINABLE incXdecY #-}

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
upperHullFromSorted :: Point_ point 2 R => NonEmpty point -> NonEmpty point
upperHullFromSorted = \case
  h@(_ :| [])  -> h
  pts          -> hull' $ NonEmpty.toList pts
--{-# INLINABLE upperHullFromSorted  #-}


-- | Computes the upper hull from a sorted input. Removes the last vertical segment.
--
--
-- running time: \(O(n)\).
upperHullFromSorted' :: Point_ point 2 R => NonEmpty point -> NonEmpty point
upperHullFromSorted' = dropVertical . upperHullFromSorted
--{-# INLINABLE upperHullFromSorted'  #-}


-- | Precondition: The list of input points is sorted
hull'          :: Point_ point 2 R => [point] -> NonEmpty point
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
--{-# INLINABLE hull' #-}

rightTurn       :: Point_ point 2 R => point -> point -> point -> Bool
rightTurn a b c = ccw a b c == CW
--{-# INLINABLE rightTurn  #-}
