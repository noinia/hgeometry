--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Plane.LowerEnvelope.Primitives
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- computing intersections of planes
--
--------------------------------------------------------------------------------
module HGeometry.Plane.LowerEnvelope.Connected.Primitives
  ( intersectionPoint
  , intersectionLine
  , intersectionVector
  ) where

import           Control.Lens
import           HGeometry.Combinatorial.Util
import           HGeometry.HyperPlane.Class
import           HGeometry.HyperPlane.NonVertical
import           HGeometry.Intersection
import           HGeometry.Line
import           HGeometry.Line.General
import           HGeometry.Point
import           HGeometry.Vector
import           HGeometry.HyperPlane.Intersection

--------------------------------------------------------------------------------
-- * Geometric Primitives


-- | Given two planes, computes the line in which they intersect.
intersectionLine      :: (Plane_ plane r, Fractional r, Eq r)
                      => plane -> plane
                      -> Maybe (VerticalOrLineEQ r)
intersectionLine h h' = do Plane_x_Plane_Line l <- planePlaneIntersection h h'
                           pure l

-- -- | Computes the directed line in which the two planes h and h' intersect. The returned
-- -- line will have h to its left and h' to its right.
-- --
-- intersectionLine'      :: ( Plane_ plane r, Ord r, Fractional r)
--                        => plane -> plane -> Maybe (LinePV 2 r)
-- intersectionLine' h h' = intersectionLine h h' <&> \case
--     VerticalLineThrough x -> reorient (LinePV (Point2 x 0) (Vector2 0 1)) (Point2 (x-1) 0)
--     NonVertical l         -> let l'@(LinePV p _) = fromLineEQ l
--                              in reorient l' (p&yCoord %~ (+1))
--   where
--     -- make sure h is to the left of the line
--     reorient l q = let f = evalAt q
--                    in if f h <= f h' then l else l&direction %~ negated
--     fromLineEQ (LineEQ a b) = fromLinearFunction a b


-- | Computes the direction vector v of the directed line l in which the two planes h and h'
-- intersect, and so that h will be to the left of the directed line
intersectionVector      :: ( Plane_ plane r, Ord r, Fractional r)
                        => plane -> plane -> Maybe (Vector 2 r)
intersectionVector h h' = intersectionLine h h' <&> \case
    VerticalLineThrough x    -> orient (Point2 (x-1) 0)     (Vector2 0 1)
    NonVertical (LineEQ a b) -> orient (Point2 0     (b+1)) (Vector2 1 a)
  where
    orient q v = let f = evalAt q in if f h <= f h' then v else negated v

-- | Computes there the three planes intersect
intersectionPoint                                    :: ( Plane_ plane r, Ord r, Fractional r)
                                                     => Three plane -> Maybe (Point 3 r)
intersectionPoint (Three h1@(Plane_ a1 b1 c1) h2 h3) =
    do l12 <- intersectionLine h1 h2
       l13 <- intersectionLine h1 h3
       case (l12,l13) of
         (VerticalLineThrough _x12, VerticalLineThrough _x13) -> Nothing
           -- if the xes are the same they would be the same plane even
         (VerticalLineThrough x, NonVertical l)               -> vertNonVertIntersect x l
         (NonVertical l, VerticalLineThrough x)               -> vertNonVertIntersect x l
         (NonVertical l, NonVertical m)                       -> l `intersect` m >>= \case
           Line_x_Line_Point (Point2 x y) -> Just $ Point3 x y (a1 * x + b1* y + c1)
           Line_x_Line_Line _             -> Nothing
   where
     vertNonVertIntersect x l = let y = evalAt' x l
                                    z = a1 * x + b1* y + c1
                                in Just $ Point3 x y z
