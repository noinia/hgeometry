module Data.Geometry.Polygon.Convex.LowerTangent( lowerTangent
                                                , upperTangent
                                                ) where

import           Control.Lens hiding ((:<), (:>))
import           Data.Vector.Circular (CircularVector)
import qualified Data.Vector.Circular as CV
import           Data.Ext
import           Data.Geometry.LineSegment
import           Data.Geometry.Point
import           Data.Geometry.Polygon (outerBoundary)
import           Data.Geometry.Polygon.Convex(ConvexPolygon(..), simplePolygon)
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
      | CV.head z   `isRightOf` (CV.head x, CV.head y) = rotate x   z (pred' z) z''
                                                      -- rotate the right polygon CCW
      | CV.head z'' `isRightOf` (CV.head x, CV.head y) = rotate z'' y z         (succ' z'')
                                                      -- rotate the left polygon CW
      | otherwise                                = ClosedLineSegment (CV.head x)
                                                                     (CV.head y)



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
      | CV.head z  `isLeftOf` (CV.head x, CV.head y) = rotate x  z (succ' z) z'
                                                    -- rotate the right polygon CW
      | CV.head z' `isLeftOf` (CV.head x, CV.head y) = rotate z' y z        (pred' z')
                                                    -- rotate the left polygon CCW
      | otherwise                              = ClosedLineSegment (CV.head x)
                                                                   (CV.head y)

--------------------------------------------------------------------------------
-- * Helper Stuff

succ' :: CircularVector a -> CircularVector a
succ' = CV.rotateRight 1

pred' :: CircularVector a -> CircularVector a
pred' = CV.rotateLeft 1

-- | Rotate to the rightmost point (rightmost and topmost in case of ties)
rightMost :: Ord r => CircularVector (Point 2 r :+ p) -> CircularVector (Point 2 r :+ p)
rightMost = CV.rotateToMaximumBy (comparing (^.core))

-- | Rotate to the leftmost point (and bottommost in case of ties)
leftMost :: Ord r => CircularVector (Point 2 r :+ p) -> CircularVector (Point 2 r :+ p)
leftMost = CV.rotateToMinimumBy (comparing (^.core))

-- | Helper to get the vertices of a convex polygon
getVertices :: ConvexPolygon p r -> CircularVector (Point 2 r :+ p)
getVertices = view (simplePolygon.outerBoundary)

isRightOf           :: (Num r, Ord r)
                    => Point 2 r :+ p -> (Point 2 r :+ p', Point 2 r :+ p'') -> Bool
a `isRightOf` (b,c) = ccw (b^.core) (c^.core) (a^.core) == CW

isLeftOf            :: (Num r, Ord r)
                    => Point 2 r :+ p -> (Point 2 r :+ p', Point 2 r :+ p'') -> Bool
a `isLeftOf` (b,c) = ccw (b^.core) (c^.core) (a^.core) == CCW
