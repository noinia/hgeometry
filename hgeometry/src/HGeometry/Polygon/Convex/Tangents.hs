{-# LANGUAGE UndecidableInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Polygon.Convex.Tangents
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Computing tangents of convex polygons.
--
--------------------------------------------------------------------------------
module HGeometry.Polygon.Convex.Tangents
  ( leftTangent, rightTangent
  , lowerTangent, lowerTangent'
  , upperTangent, upperTangent'
  ) where

import           Control.Lens
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Maybe (fromMaybe)
import           Data.Monoid
import           Data.Ord (comparing)
import           Data.Semigroup.Foldable (Foldable1 (..))
import           HGeometry.Ext
-- import           Data.Vector.Circular (CircularVector)
-- import qualified Data.Vector.Circular as CV
-- import qualified Data.Vector.Circular.Util as CV
import           HGeometry.LineSegment
import           HGeometry.Point
import           HGeometry.Polygon.Class
import           HGeometry.Polygon.Convex.Class
import           HGeometry.Vector
import           HGeometry.Polygon.Convex.Implementation (findMaxWith)
--------------------------------------------------------------------------------

-- | Given a convex polygon poly, and a point outside the polygon, find the
--  left tangent of q and the polygon, i.e. the vertex v of the convex polygon
--  s.t. the polygon lies completely to the right of the line from q to v.
--
-- running time: \(O(\log n)\).
leftTangent        :: (Ord r, Num r, ConvexPolygon_ convexPolygon point r)
                   => convexPolygon -> point -> point
leftTangent poly q = findMaxWith (tangentCmp q) poly

-- | Given a convex polygon poly, and a point outside the polygon, find the
--  right tangent of q and the polygon, i.e. the vertex v of the convex polygon
--  s.t. the polygon lies completely to the left of the line from q to v.
--
-- running time: \(O(\log n)\).
rightTangent        :: (Ord r, Num r, ConvexPolygon_ convexPolygon point r)
                    => convexPolygon -> point -> point
rightTangent poly q = findMaxWith (flip $ tangentCmp q) poly


tangentCmp       :: (Num r, Ord r, Point_ point 2 r)
                 => point -> point -> point -> Ordering
tangentCmp o p q = case ccw o p q of
                     CCW      -> LT -- q is left of the line from o to p
                     CoLinear -> EQ -- q is *on* the line from o to p
                     CW       -> GT -- q is right of the line from o to p

--------------------------------------------------------------------------------
-- * Computing Tangents Of Polyons

-- | Compute the lower tangent of the two polgyons
--
--   pre: - polygons lp and rp have at least 1 vertex
--        - lp and rp are disjoint, and there is a vertical line separating
--          the two polygons.
--        - The vertices of the polygons are given in clockwise order
--
-- Running time: O(n+m), where n and m are the sizes of the two polygons respectively
lowerTangent       :: ( Num r, Ord r
                      , ConvexPolygon_ convexPolygon point r, Point_ point 2 r
                      )
                   => convexPolygon
                   -> convexPolygon
                   -> ClosedLineSegment point
lowerTangent lp rp = ClosedLineSegment l r
  where
    lh = toNonEmptyOf (cwOuterBoundaryFrom   (rightMostIdx lp)) lp
    rh = toNonEmptyOf (ccwOuterBoundaryFrom  (leftMostIdx rp))  rp
    (Vector2 (l :+ _) (r :+ _)) = lowerTangent' lh rh

-- | Index of the rightmost vertex. Returns the topmost such vertex if there are multiple
rightMostIdx :: (Ord r, ConvexPolygon_ convexPolygon point r, Point_ point 2 r)
             => convexPolygon -> Int
rightMostIdx = fst . maximum1ByOf (outerBoundary.withIndex) comparePtsXY

-- | compare on x and y coordinates
comparePtsXY             :: (Ord r, Point_ point 2 r) => (i, point) -> (i,point) -> Ordering
comparePtsXY (_,p) (_,q) = comparing (^.xCoord) p q <> comparing (^.yCoord) p q

-- | Index of the leftmost vertex, returns the bottommost if there are multiple.
leftMostIdx :: (Ord r, ConvexPolygon_ convexPolygon point r, Point_ point 2 r)
             => convexPolygon -> Int
leftMostIdx = fst . minimum1ByOf (outerBoundary.withIndex) comparePtsXY

-- | get the maximum of the elements the lens points to using the given comparison function
maximum1ByOf       :: Getting (Endo (Endo (Maybe a))) s a -> (a -> a -> Ordering) -> s -> a
maximum1ByOf l cmp = fromMaybe (error "maximum1ByOf") . maximumByOf l cmp

-- | get the minimum of the elements the lens points to using the given comparison function
minimum1ByOf       :: Getting (Endo (Endo (Maybe a))) s a -> (a -> a -> Ordering) -> s -> a
minimum1ByOf l cmp = fromMaybe (error "minimum1ByOf") . minimumByOf l cmp

-- | Compute the lower tangent of the two convex chains lp and rp
--
--   pre: - the chains lp and rp have at least 1 vertex
--        - lp and rp are disjoint, and there is a vertical line
--          having lp on the left and rp on the right.
--        - The vertices in the left-chain are given in clockwise order, (right to left)
--        - The vertices in the right chain are given in counterclockwise order (left-to-right)
--
-- The result returned is the two endpoints l and r of the tangents,
-- and the remainders lc and rc of the chains (i.e.)  such that the lower hull
-- of both chains is: (reverse lc) ++ [l,h] ++ rc
--
-- Running time: \(O(n+m)\), where n and m are the sizes of the two chains
-- respectively
lowerTangent'       :: forall point r f. (Ord r, Num r, Foldable1 f, Point_ point 2 r)
                    => f point -> f point -> Vector 2 (point :+ [point])
lowerTangent' l0 r0 = go (toNonEmpty l0) (toNonEmpty r0)
  where
    ne = NonEmpty.fromList
    isRight' []    _ _ = False
    isRight' (x:_) l r = ccw l r x /= CCW

    go lh@(l:|ls) rh@(r:|rs) | isRight' rs l r = go lh      (ne rs)
                             | isRight' ls l r = go (ne ls) rh
                             | otherwise       = Vector2 (l :+ ls) (r :+ rs)



-- -- | Given an index i, returns an IndexedTraversal of the cyclic
-- -- structure, starting from the element with index i.
-- itraverseRightFrom           :: Int -> IndexedTraversal Int (Cyclic v a) (Cyclic v b) a b
-- itraverseRightFrom i pafb cv = [0..n]
--   where
--     n = length cv




-- | Compute the upper tangent of the two polgyons
--
--   pre: - polygons lp and rp have at least 1 vertex
--        - lp and rp are disjoint, and there is a vertical line separating
--          the two polygons.
--        - The vertices of the polygons are given in clockwise order
--
-- Running time: \( O(n+m) \), where n and m are the sizes of the two polygons respectively
upperTangent       :: forall convexPolygon point r.
                      ( Num r, Ord r
                      , ConvexPolygon_ convexPolygon point r)
                   => convexPolygon
                   -> convexPolygon
                   -> ClosedLineSegment point
upperTangent lp rp = ClosedLineSegment l r
  where
    lh = toNonEmptyOf (cwOuterBoundaryFrom  (rightMostIdx lp)) lp
    rh = toNonEmptyOf (ccwOuterBoundaryFrom (leftMostIdx rp))  rp
    (Vector2 (l :+ _) (r :+ _)) = upperTangent' lh rh

-- | Compute the upper tangent of the two convex chains lp and rp
--
--   pre: - the chains lp and rp have at least 1 vertex
--        - lp and rp are disjoint, and there is a vertical line
--          having lp on the left and rp on the right.
--        - The vertices in the left-chain are given in clockwise order, (right to left)
--        - The vertices in the right chain are given in counterclockwise order (left-to-right)
--
-- The result returned is the two endpoints l and r of the tangents,
-- and the remainders lc and rc of the chains (i.e.)  such that the upper hull
-- of both chains is: (reverse lc) ++ [l,h] ++ rc
--
-- Running time: \(O(n+m)\), where n and m are the sizes of the two chains
-- respectively
upperTangent'       :: (Ord r, Num r, Foldable1 f, Point_ point 2 r)
                    => f point -> f point -> Vector 2 (point :+ [point])
upperTangent' l0 r0 = go (toNonEmpty l0) (toNonEmpty r0)
  where
    ne = NonEmpty.fromList
    isLeft' []    _ _ = False
    isLeft' (x:_) l r = ccw l r x /= CW

    go lh@(l:|ls) rh@(r:|rs) | isLeft' rs l r = go lh      (ne rs)
                             | isLeft' ls l r = go (ne ls) rh
                             | otherwise      = Vector2 (l :+ ls) (r :+ rs)
