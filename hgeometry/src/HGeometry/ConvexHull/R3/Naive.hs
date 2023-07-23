--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.ConvexHull.R3.Naive
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--------------------------------------------------------------------------------
module HGeometry.ConvexHull.R3.Naive
  ( ConvexHull
  , lowerHull', lowerHullAll

  , isValidTriangle, upperHalfSpaceOf
  ) where

import Control.Lens
import Data.Foldable (toList)
import Data.List (find)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (isNothing)
import HGeometry.Combinatorial.Util
import HGeometry.HalfSpace
import HGeometry.HyperPlane
import HGeometry.Intersection (intersects)
import HGeometry.Point
import HGeometry.Triangle
import HGeometry.Vector

--------------------------------------------------------------------------------

type ConvexHull point = [Triangle point]

-- | Computes the lower hull without its vertical triangles.
--
-- pre: The points are in general position. In particular, no four
-- points should be coplanar.
--
-- running time: \(O(n^4)\)
lowerHull' :: forall point r. (Ord r, Fractional r, Show r, Point_ point 3 r)
           => NonEmpty point -> ConvexHull point
lowerHull' = filter (not . isVertical) . lowerHullAll
  where
    toPt2 :: point ->  Point 2 r
    toPt2 = projectPoint

    isVertical (Triangle p q r) = ccw (toPt2 p) (toPt2 q) (toPt2 r) == CoLinear

-- | Generates a set of triangles to be used to construct a complete
-- convex hull. In particular, it may contain vertical triangles.
--
-- pre: The points are in general position. In particular, no four
-- points should be coplanar.
--
-- running time: \(O(n^4)\)
lowerHullAll                 :: (Ord r, Fractional r, Show r, Point_ point 3 r)
                             => NonEmpty point -> ConvexHull point
lowerHullAll (toList -> pts) = let mkT (Three p q r) = Triangle p q r in
    [ t | t <- mkT <$> uniqueTriplets pts, isNothing (isValidTriangle t pts) ]



_killOverlapping :: ( Ord r, Fractional r
                    , Point_ point 3 r
                    ) => [Triangle point] -> [Triangle point]
_killOverlapping = foldr keepIfNotOverlaps []
  where
    keepIfNotOverlaps t ts | any (t `overlaps`) ts = ts
                           | otherwise             = t:ts

overlaps :: (Fractional r, Ord r, Point_ point 3 r) => Triangle point -> Triangle point -> Bool
t1 `overlaps` t2 = upperHalfSpaceOf t1 == upperHalfSpaceOf t2 && False


-- | Tests if this is a valid triangle for the lower envelope. That
-- is, if all point lie above the plane through these points. Returns
-- a Maybe; if the result is a Nothing the triangle is valid, if not
-- it returns a counter example.
--
-- >>> let t = (Triangle origin (Point3 1 0 0) (Point3 0 1 0))
-- >>> isValidTriangle t [Point3 5 5 0]
-- Nothing
-- >>> let t = (Triangle origin (Point3 1 0 0) (Point3 0 1 0))
-- >>> isValidTriangle t [Point3 5 5 (-10)]
-- Just (Point3 5 5 (-10))
isValidTriangle   :: (Num r, Ord r, Point_ point 3 r)
                  => Triangle point -> [point] -> Maybe point
isValidTriangle t = find (\q -> not $ (q^.asPoint) `intersects` h)
  where
    h = upperHalfSpaceOf t


-- | Computes the halfspace above the triangle.
--
-- >>> upperHalfSpaceOf (Triangle origin (Point3 10 0 0) (Point3 0 10 0))
-- HalfSpace Positive (HyperPlane [0,0,0,100])
upperHalfSpaceOf                  :: (Ord r, Num r, Point_ point 3 r)
                                  => Triangle point -> HalfSpace 3 r
upperHalfSpaceOf (Triangle p q r) = HalfSpace Positive h
  where
    h = hyperPlaneThrough $ Vector3 p q r
    -- c = p&zCoord -~ 1
    -- s  = if c `onSideTest` h /= GT then Positive else Negative
