module Data.Geometry.Polygon.Convex where

import Control.Lens
import Data.Ext
import Data.Function(on, )
import Data.Geometry
import Data.Geometry.Polygon(fromPoints)
import qualified Data.CircularList as C
import qualified Data.Foldable as F
import Data.Maybe(fromJust)
import Data.Ord(comparing)


import Data.Geometry.Ipe
import Debug.Trace

--------------------------------------------------------------------------------

type ConvexPolygon = SimplePolygon



-- | Rotating Right <-> rotate clockwise

-- Implementation of the Divide & Conqueror algorithm as described in:
--
-- Two Algorithms for Constructing a Delaunay Triangulation
-- Lee and Schachter
-- International Journal of Computer and Information Sciences, Vol 9, No. 3, 1980


-- : (combined hull, lower tangent that was added, upper tangent thtat was
-- added)

-- pre: - lp and rp are disjoint, and there is a vertical line separating
--        the two polygons.
--      - The vertices of the polygons are given in clockwise order
--
-- Running time: O(n+m), where n and m are the sizes of the two polygons respectively
merge       :: (Num r, Ord r) => ConvexPolygon p r  -> ConvexPolygon p r
            -> (ConvexPolygon p r, LineSegment 2 p r, LineSegment 2 p r)
merge lp rp = (fromPoints $ r' ++ l', lt, ut)
  where
    lt@(ClosedLineSegment a b) = lowerTangent lp rp
    ut@(ClosedLineSegment c d) = upperTangent lp rp


    takeUntil p xs = let (xs',x:_) = break p xs in xs' ++ [x]

    r' = takeUntil (coreEq b) . C.rightElements . rotateTo' d $ rp^.outerBoundary
    l' = takeUntil (coreEq c) . C.rightElements . rotateTo' a $ lp^.outerBoundary


rotateTo'   :: Eq a => (a :+ b) -> C.CList (a :+ b) -> C.CList (a :+ b)
rotateTo' x = fromJust . C.findRotateTo (coreEq x)


coreEq :: Eq a => (a :+ b) -> (a :+ b) -> Bool
coreEq = (==) `on` (^.core)


trace'    :: Show a => String -> a -> a
trace' s a = trace ("|||" ++ s ++ " " ++ show a ++ "|||\n") a

-- | Compute the lower tangent of the two polgyons
--
--   pre: - polygons lp and rp have at least 1 vertex
--        - lp and rp are disjoint, and there is a vertical line separating
--          the two polygons.
--        - The vertices of the polygons are given in clockwise order
--
-- Running time: O(n+m), where n and m are the sizes of the two polygons respectively
lowerTangent                                     :: (Num r, Ord r)
                                                 => ConvexPolygon p r
                                                 -> ConvexPolygon p r
                                                 -> LineSegment 2 p r
lowerTangent (SimplePolygon l) (SimplePolygon r) = rotate xx yy zz zz''
  where
    xx = rightMost l
    yy = leftMost r

    zz   = pred' yy
    zz'' = succ' xx

    rotate x y z z''
      | focus' z   `isRightOf` (focus' x, focus' y) = rotate x   z (pred' z) z''
                                                      -- rotate the right polygon CCW
      | focus' z'' `isRightOf` (focus' x, focus' y) = rotate z'' y z         (succ' z'')
                                                      -- rotate the left polygon CW
      | otherwise                                   = ClosedLineSegment (focus' x)
                                                                        (focus' y)

focus' :: C.CList c -> c
focus' = fromJust . C.focus

succ' :: C.CList a -> C.CList a
succ' = C.rotR

pred' :: C.CList a -> C.CList a
pred' = C.rotL

-- | Compute the upper tangent of the two polgyons
--
--   pre: - polygons lp and rp have at least 1 vertex
--        - lp and rp are disjoint, and there is a vertical line separating
--          the two polygons.
--        - The vertices of the polygons are given in clockwise order
--
-- Running time: O(n+m), where n and m are the sizes of the two polygons respectively
upperTangent                                     :: (Num r, Ord r)
                                                 => ConvexPolygon p r
                                                 -> ConvexPolygon p r
                                                 -> LineSegment 2 p r
upperTangent (SimplePolygon l) (SimplePolygon r) = rotate xx yy zz zz'
  where
    xx = rightMost l
    yy = leftMost r

    zz  = succ' yy
    zz' = pred' xx

    rotate x y z z'
      | focus' z  `isLeftOf` (focus' x, focus' y) = rotate x  z (succ' z) z'
                                                    -- rotate the right polygon CW
      | focus' z' `isLeftOf` (focus' x, focus' y) = rotate z' y z        (pred' z')
                                                    -- rotate the left polygon CCW
      | otherwise                                 = ClosedLineSegment (focus' x)
                                                                      (focus' y)

isRightOf           :: (Num r, Ord r)
                    => Point 2 r :+ p -> (Point 2 r :+ p', Point 2 r :+ p'') -> Bool
a `isRightOf` (b,c) = ccw (b^.core) (c^.core) (a^.core) == CW

isLeftOf            :: (Num r, Ord r)
                    => Point 2 r :+ p -> (Point 2 r :+ p', Point 2 r :+ p'') -> Bool
a `isLeftOf` (b,c) = ccw (b^.core) (c^.core) (a^.core) == CCW


--------------------------------------------------------------------------------

-- | Rotate to the rightmost point
rightMost    :: Ord r => C.CList (Point 2 r :+ p) -> C.CList (Point 2 r :+ p)
rightMost xs = let m = F.maximumBy (comparing (^.core.xCoord)) xs in rotateTo' m xs

-- | Rotate to the leftmost point
leftMost    :: Ord r => C.CList (Point 2 r :+ p) -> C.CList (Point 2 r :+ p)
leftMost xs = let m = F.minimumBy (comparing (^.core.xCoord)) xs in rotateTo' m xs


-- | rotate right while p 'current' 'rightNeibhour' is true
rotateRWhile      :: (a -> a -> Bool) -> C.CList a -> C.CList a
rotateRWhile p lst
  | C.isEmpty lst = lst
  | otherwise     = go lst
    where
      go xs = let cur = focus' xs
                  xs' = C.rotR xs
                  nxt = focus' xs'
              in if p cur nxt then go xs' else xs


left = fromPoints . map (input !! ) $ [3, 0, 1]
right = fromPoints . map (input !!) $ [7,8,6,5,4]

test = merge left right

input :: [Point 2 Rational :+ ()]
input = map ext [ point2 64 656
                , point2 64  736
                , point2 96 688
                , point2 128 672
                , point2 128 752
                , point2 160 704
                , point2 192 672
                , point2 192 736
                , point2 208 704
                ]
