module Data.Geometry.Polygon.Convex.LowerTangent( lowerTangent
                                                , upperTangent
                                                ) where

import           Control.Lens hiding ((:<), (:>))
import           Data.CircularSeq (focus,CSeq)
import qualified Data.CircularSeq as C
import           Data.Ext
import qualified Data.Foldable as F
import           Data.Function (on)
import           Data.Geometry.LineSegment
import           Data.Geometry.Point
import           Data.Geometry.Polygon (outerBoundary)
import           Data.Geometry.Polygon.Convex(ConvexPolygon(..), simplePolygon)
import           Data.Maybe (fromJust)
import           Data.Ord (comparing)


-- Old implementation of lowerTangent that we know is correct.

-- | Compute the lower tangent of the two polgyons
--
--   pre: - polygons lp and rp have at least 1 vertex
--        - lp and rp are disjoint, and there is a vertical line separating
--          the two polygons.
--        - The vertices of the polygons are given in clockwise order
--
-- Running time: O(n+m), where n and m are the sizes of the two polygons respectively
lowerTangent                                       :: (Num r, Ord r)
                                                   => ConvexPolygon p r
                                                   -> ConvexPolygon p r
                                                   -> LineSegment 2 p r
lowerTangent (getVertices -> l) (getVertices -> r) = rotate xx yy zz zz''
  where
    xx = rightMost l
    yy = leftMost r

    zz   = pred' yy
    zz'' = succ' xx


    rotate x y z z''
      | focus z   `isRightOf` (focus x, focus y) = rotate x   z (pred' z) z''
                                                      -- rotate the right polygon CCW
      | focus z'' `isRightOf` (focus x, focus y) = rotate z'' y z         (succ' z'')
                                                      -- rotate the left polygon CW
      | otherwise                                = ClosedLineSegment (focus x)
                                                                     (focus y)



-- | Compute the upper tangent of the two polgyons
--
--   pre: - polygons lp and rp have at least 1 vertex
--        - lp and rp are disjoint, and there is a vertical line separating
--          the two polygons.
--        - The vertices of the polygons are given in clockwise order
--
-- Running time: O(n+m), where n and m are the sizes of the two polygons respectively
upperTangent                                       :: (Num r, Ord r)
                                                   => ConvexPolygon p r
                                                   -> ConvexPolygon p r
                                                   -> LineSegment 2 p r
upperTangent (getVertices -> l) (getVertices -> r) = rotate xx yy zz zz'
  where
    xx = rightMost l
    yy = leftMost r

    zz  = succ' yy
    zz' = pred' xx

    rotate x y z z'
      | focus z  `isLeftOf` (focus x, focus y) = rotate x  z (succ' z) z'
                                                    -- rotate the right polygon CW
      | focus z' `isLeftOf` (focus x, focus y) = rotate z' y z        (pred' z')
                                                    -- rotate the left polygon CCW
      | otherwise                              = ClosedLineSegment (focus x)
                                                                   (focus y)

--------------------------------------------------------------------------------
-- * Helper Stuff

succ' :: CSeq a -> CSeq a
succ' = C.rotateR

pred' :: CSeq a -> CSeq a
pred' = C.rotateL

-- | Rotate to the rightmost point (rightmost and topmost in case of ties)
rightMost    :: Ord r => CSeq (Point 2 r :+ p) -> CSeq (Point 2 r :+ p)
rightMost xs = let m = F.maximumBy (comparing (^.core)) xs in rotateTo' m xs

-- | Rotate to the leftmost point (and bottommost in case of ties)
leftMost    :: Ord r => CSeq (Point 2 r :+ p) -> CSeq (Point 2 r :+ p)
leftMost xs = let m = F.minimumBy (comparing (^.core)) xs in rotateTo' m xs

-- | Helper to get the vertices of a convex polygon
getVertices :: ConvexPolygon p r -> CSeq (Point 2 r :+ p)
getVertices = view (simplePolygon.outerBoundary)

isRightOf           :: (Num r, Ord r)
                    => Point 2 r :+ p -> (Point 2 r :+ p', Point 2 r :+ p'') -> Bool
a `isRightOf` (b,c) = ccw (b^.core) (c^.core) (a^.core) == CW

isLeftOf            :: (Num r, Ord r)
                    => Point 2 r :+ p -> (Point 2 r :+ p', Point 2 r :+ p'') -> Bool
a `isLeftOf` (b,c) = ccw (b^.core) (c^.core) (a^.core) == CCW

rotateTo'   :: Eq a => (a :+ b) -> CSeq (a :+ b) -> CSeq (a :+ b)
rotateTo' x = fromJust . C.findRotateTo (coreEq x)


coreEq :: Eq a => (a :+ b) -> (a :+ b) -> Bool
coreEq = (==) `on` (^.core)
