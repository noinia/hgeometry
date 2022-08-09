module Geometry.Polygon.Convex.Tangents
  ( leftTangent, rightTangent
  , lowerTangent, lowerTangent'
  , upperTangent, upperTangent'

  , findMaxWith
  ) where

import           Control.Lens ((^.),(^..))
import           Data.Ext
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Ord (comparing)
import           Data.Semigroup.Foldable (Foldable1 (..))
import           Data.Util
import           Data.Vector.Circular (CircularVector)
import qualified Data.Vector.Circular as CV
import qualified Data.Vector.Circular.Util as CV
import           Geometry.LineSegment.Boxed
import           Geometry.Point
import           Geometry.Polygon.Class
import           Geometry.Polygon.Convex.Class

--------------------------------------------------------------------------------

-- | Given a convex polygon poly, and a point outside the polygon, find the
--  left tangent of q and the polygon, i.e. the vertex v of the convex polygon
--  s.t. the polygon lies completely to the right of the line from q to v.
--
-- running time: \(O(\log n)\).
leftTangent        :: (Ord r, Num r, ConvexPolygon_ convexPolygon point r)
                   => convexPolygon point r -> point 2 r -> point 2 r
leftTangent poly q = findMaxWith (tangentCmp q) poly

-- | Given a convex polygon poly, and a point outside the polygon, find the
--  right tangent of q and the polygon, i.e. the vertex v of the convex polygon
--  s.t. the polygon lies completely to the left of the line from q to v.
--
-- running time: \(O(\log n)\).
rightTangent        :: (Ord r, Num r, ConvexPolygon_ convexPolygon point r)
                    => convexPolygon point r -> point 2 r -> point 2 r
rightTangent poly q = findMaxWith (flip $ tangentCmp q) poly


tangentCmp       :: (Num r, Ord r, Point_ point 2 r)
                 => point 2 r -> point 2 r -> point 2 r -> Ordering
tangentCmp o p q = case ccw o p q of
                     CCW      -> LT -- q is left of the line from o to p
                     CoLinear -> EQ -- q is *on* the line from o to p
                     CW       -> GT -- q is right of the line from o to p


-- | Find the maximum vertex in a convex polygon using a binary search.
-- \( O(\log n) \)
findMaxWith :: ( ConvexPolygon_ convexPolygon point r
               )
            => (point 2 r -> point 2 r -> Ordering)
             -> convexPolygon point r -> point 2 r
findMaxWith cmp p = p^.outerBoundaryVertexAt (worker 0 (numVertices p))
  where
    a `icmp` b = (p^.outerBoundaryVertexAt a) `cmp` (p^.outerBoundaryVertexAt b)
    worker a b
      | localMaximum c = c
      | a+1==b         = b
      | otherwise      =
        case  (isUpwards a, isUpwards c, c `icmp` a /= LT) of
          (True, False, _)      -> worker a c -- A is up, C is down, pick [a,c]
          (True, True, True)    -> worker c b -- A is up, C is up, C is GTE A, pick [c,b]
          (True, True, False)   -> worker a c -- A is up, C is LT A, pick [a,c]
          (False, True, _)      -> worker c b -- A is down, C is up, pick [c,b]
          (False, False, False) -> worker c b -- A is down, C is down, C is LT A, pick [c,b]
          (False, _, True)      -> worker a c -- A is down, C is GTE A, pick [a,c]
      where
        c = (a+b) `div` 2
        localMaximum idx = idx `icmp` (c-1) == GT && idx `icmp` (c+1) == GT
    isUpwards idx = idx `icmp` (idx+1) /= GT
  -- FIXME: c+1 is always less than n so we don't need to use `mod` or do bounds checking.
  --        Use unsafe indexing.

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
lowerTangent       :: (Num r, Ord r, ConvexPolygon_ convexPolygon point r)
                   => convexPolygon point r
                   -> convexPolygon point r
                   -> ClosedLineSegment 2 point r
lowerTangent lp rp = ClosedLineSegment l r
  where
    lh = CV.rightElements . rightMost . getVertices $ lp
    rh = CV.leftElements  . leftMost  . getVertices $ rp
    (Two (l :+ _) (r :+ _)) = lowerTangent' lh rh


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
                    => f (point 2 r) -> f (point 2 r) -> Two (point 2 r :+ [point 2 r])
lowerTangent' l0 r0 = go (toNonEmpty l0) (toNonEmpty r0)
  where
    ne = NonEmpty.fromList
    isRight' []    _ _ = False
    isRight' (x:_) l r = ccw l r x /= CCW

    go lh@(l:|ls) rh@(r:|rs) | isRight' rs l r = go lh      (ne rs)
                             | isRight' ls l r = go (ne ls) rh
                             | otherwise       = Two (l :+ ls) (r :+ rs)

-- | Compute the upper tangent of the two polgyons
--
--   pre: - polygons lp and rp have at least 1 vertex
--        - lp and rp are disjoint, and there is a vertical line separating
--          the two polygons.
--        - The vertices of the polygons are given in clockwise order
--
-- Running time: \( O(n+m) \), where n and m are the sizes of the two polygons respectively
upperTangent       :: forall convexPolygon point r. (Num r, Ord r, ConvexPolygon_ convexPolygon point r)
                   => convexPolygon point r
                   -> convexPolygon point r
                   -> ClosedLineSegment 2 point r
upperTangent lp rp = ClosedLineSegment l r
  where
    lh = CV.leftElements  . rightMost . getVertices $ lp
    rh = CV.rightElements . leftMost  . getVertices $ rp
    (Two (l :+ _) (r :+ _)) = upperTangent' lh rh

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
                    => f (point 2 r) -> f (point 2 r) -> Two (point 2 r :+ [point 2 r])
upperTangent' l0 r0 = go (toNonEmpty l0) (toNonEmpty r0)
  where
    ne = NonEmpty.fromList
    isLeft' []    _ _ = False
    isLeft' (x:_) l r = ccw l r x /= CW

    go lh@(l:|ls) rh@(r:|rs) | isLeft' rs l r = go lh      (ne rs)
                             | isLeft' ls l r = go (ne ls) rh
                             | otherwise      = Two (l :+ ls) (r :+ rs)



--------------------------------------------------------------------------------


-- | Rotate to the rightmost point (rightmost and topmost in case of ties)
rightMost :: (Ord r, Point_ point 2 r)
          => CircularVector (point 2 r) -> CircularVector (point 2 r)
rightMost = CV.rotateToMaximumBy (comparing $ \p -> (p^.xCoord,p^.yCoord))

-- | Rotate to the leftmost point (and bottommost in case of ties)
leftMost :: (Ord r, Point_ point 2 r)
         => CircularVector (point 2 r) -> CircularVector (point 2 r)
leftMost = CV.rotateToMinimumBy (comparing $ \p -> (p^.xCoord,p^.yCoord))

-- | get the vertices of a polygon as a circularvector
getVertices   :: HasOuterBoundary polygon => polygon -> CircularVector (Vertex polygon)
getVertices p = CV.unsafeFromListN (numVertices p) (p^..outerBoundary)
