{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module    : Data.Geometry.Polygon.Convex
Description: Convex Polygons
Copyright : (c) Frank Staals
License : See LICENCE file
-}
module Data.Geometry.Polygon.Convex( ConvexPolygon
                                   , merge
                                   , lowerTangent, upperTangent
                                   , isLeftOf, isRightOf

                                   , extremes
                                   , maxInDirection

                                   , leftTangent, rightTangent

                                   ) where

import           Control.Lens hiding ((:<), (:>))
import           Data.CircularSeq (focus,CSeq)
import qualified Data.CircularSeq as C
import           Data.Ext
import qualified Data.Foldable as F
import           Data.Function (on, )
import           Data.Geometry
import           Data.Geometry.Polygon (fromPoints, cmpExtreme)
import           Data.Maybe (fromJust)
import           Data.Ord (comparing)
import           Data.Sequence (viewl,viewr, ViewL(..), ViewR(..))
import qualified Data.Sequence as S

import           Data.Geometry.Ipe
import           Debug.Trace
--------------------------------------------------------------------------------

type ConvexPolygon = SimplePolygon


mainWith inFile outFile = do
    ePage <- readSinglePageFile inFile
    case ePage of
      Left err                         -> error "" -- err
      Right (page :: IpePage Rational) -> case page^..content.traverse._withAttrs _IpePath _asSimplePolygon.core of
        []         -> error "No points found"
        polies@(_:_) -> do
           -- let out  = [asIpe drawTriangulation dt, asIpe drawTree' emst]
           -- print $ length $ edges' dt
           -- print $ toPlaneGraph (Proxy :: Proxy DT) dt
           -- writeIpeFile outFile . singlePageFromContent $ out
           -- mapM_ (print . extremesNaive (v2 1 0)) polies
           pure $ map (flip rightTangent (point2 80 528)) polies




-- | Finds the extreme points, minimum and maximum, in a given direction
--
-- pre: The input polygon is strictly convex.
--
-- running time: $O(\log n)$
--
--
extremes     :: (Num r, Ord r) => Vector 2 r -> ConvexPolygon p r
             -> (Point 2 r :+ p, Point 2 r :+ p)
extremes u p = (maxInDirection ((-1) *^ u) p, maxInDirection u p)

-- | Finds the extreme maximum point in the given direction. Based on
-- http://geomalgorithms.com/a14-_extreme_pts.html
--
--
-- pre: The input polygon is strictly convex.
--
-- running time: $O(\log^2 n)$
maxInDirection   :: (Num r, Ord r) => Vector 2 r -> ConvexPolygon p r -> Point 2 r :+ p
maxInDirection u = findMaxWith (cmpExtreme u)

findMaxWith       :: (Point 2 r :+ p -> Point 2 r :+ p -> Ordering)
                  -> ConvexPolygon p r -> Point 2 r :+ p
findMaxWith cmp p = findMaxStart . C.rightElements $ p^.outerBoundary
  where
    p' >=. q = (p' `cmp` q) /= LT

    findMaxStart s@(viewl -> (a:<r))
      | isLocalMax r a r = a
      | otherwise        = findMax s
    findMaxStart _       = error "absurd"

    findMax s = let i         = F.length s `div` 2
                    (ac,cb')  = S.splitAt i s
                    (c :< cb) = viewl cb'
                in findMax' ac c cb

    findMax' ac c cb
      | isLocalMax ac c cb = c
      | otherwise          = binSearch ac c cb

    -- | Given the vertices [a..] c [..b] find the exteral vtx
    binSearch ac@(viewl -> a:<r) c cb = case (isUpwards a r, isUpwards c cb, a >=. c) of
        (True,False,_)      -> findMax (ac |> c)
        (True,True,True)    -> findMax (ac |> c)
        (True,True,False)   -> findMax (c <| cb)

        (False,True,_)      -> findMax (c <| cb)
        (False,False,False) -> findMax (ac |> c)
        (False,False,True)  -> findMax (c <| cb)
    binSearch _                  _ _ = error "maxInDirection, binSearch: empty chain"

    isLocalMax (viewr -> _ :> l) c (viewl -> r :< _) = c >=. l && c >=. r
    isLocalMax (viewr -> _ :> l) c _                 = c >=. l
    isLocalMax _                 c (viewl -> r :< _) = c >=. r
    isLocalMax _                 _ _                 = True

    -- the Edge from a to b is upwards w.r.t b if a is not larger than b
    isUpwards a (viewl -> b :< _) = (a `cmp` b) /= GT
    isUpwards _ _                 = error "isUpwards: no edge endpoint"


tangentCmp       :: (Num r, Ord r)
                 => Point 2 r -> Point 2 r :+ p -> Point 2 r :+ q -> Ordering
tangentCmp o p q = case ccw o (p^.core) (q^.core) of
                     CCW      -> LT -- q is left of the line from o to p
                     CoLinear -> EQ -- q is *on* the line from o to p
                     CW       -> GT -- q is right of the line from o to p


--  | Given a convex polygon poly, and a point outside the polygon, find the
--  left tangent of q and the polygon, i.e. the vertex v of the convex polygon
--  s.t. the polygon lies completely to the right of the line from q to v.
--
-- running time: $O(\log^2 n)$.
leftTangent        :: (Ord r, Num r) => ConvexPolygon p r -> Point 2 r -> Point 2 r :+ p
leftTangent poly q = findMaxWith (tangentCmp q) poly

--  | Given a convex polygon poly, and a point outside the polygon, find the
--  right tangent of q and the polygon, i.e. the vertex v of the convex polygon
--  s.t. the polygon lies completely to the left of the line from q to v.
--
-- running time: $O(\log^2 n)$.
rightTangent        :: (Ord r, Num r) => ConvexPolygon p r -> Point 2 r -> Point 2 r :+ p
rightTangent poly q = findMaxWith (flip $ tangentCmp q) poly






-- * Merging Two convex Hulls


-- | Rotating Right <-> rotate clockwise
--
-- Merging two convex hulls, based on the paper:
--
-- Two Algorithms for Constructing a Delaunay Triangulation
-- Lee and Schachter
-- International Journal of Computer and Information Sciences, Vol 9, No. 3, 1980
--
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
    rightElems  = F.toList . C.rightElements

    r' = takeUntil (coreEq b) . rightElems . rotateTo' d $ rp^.outerBoundary
    l' = takeUntil (coreEq c) . rightElems . rotateTo' a $ lp^.outerBoundary


rotateTo'   :: Eq a => (a :+ b) -> CSeq (a :+ b) -> CSeq (a :+ b)
rotateTo' x = fromJust . C.findRotateTo (coreEq x)


coreEq :: Eq a => (a :+ b) -> (a :+ b) -> Bool
coreEq = (==) `on` (^.core)

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
      | focus z   `isRightOf` (focus x, focus y) = rotate x   z (pred' z) z''
                                                      -- rotate the right polygon CCW
      | focus z'' `isRightOf` (focus x, focus y) = rotate z'' y z         (succ' z'')
                                                      -- rotate the left polygon CW
      | otherwise                                = ClosedLineSegment (focus x)
                                                                     (focus y)

succ' :: CSeq a -> CSeq a
succ' = C.rotateR

pred' :: CSeq a -> CSeq a
pred' = C.rotateL

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
      | focus z  `isLeftOf` (focus x, focus y) = rotate x  z (succ' z) z'
                                                    -- rotate the right polygon CW
      | focus z' `isLeftOf` (focus x, focus y) = rotate z' y z        (pred' z')
                                                    -- rotate the left polygon CCW
      | otherwise                              = ClosedLineSegment (focus x)
                                                                   (focus y)

isRightOf           :: (Num r, Ord r)
                    => Point 2 r :+ p -> (Point 2 r :+ p', Point 2 r :+ p'') -> Bool
a `isRightOf` (b,c) = ccw (b^.core) (c^.core) (a^.core) == CW

isLeftOf            :: (Num r, Ord r)
                    => Point 2 r :+ p -> (Point 2 r :+ p', Point 2 r :+ p'') -> Bool
a `isLeftOf` (b,c) = ccw (b^.core) (c^.core) (a^.core) == CCW


--------------------------------------------------------------------------------

-- | Rotate to the rightmost point
rightMost    :: Ord r => CSeq (Point 2 r :+ p) -> CSeq (Point 2 r :+ p)
rightMost xs = let m = F.maximumBy (comparing (^.core.xCoord)) xs in rotateTo' m xs

-- | Rotate to the leftmost point
leftMost    :: Ord r => CSeq (Point 2 r :+ p) -> CSeq (Point 2 r :+ p)
leftMost xs = let m = F.minimumBy (comparing (^.core.xCoord)) xs in rotateTo' m xs


-- -- | rotate right while p 'current' 'rightNeibhour' is true
-- rotateRWhile      :: (a -> a -> Bool) -> C.CList a -> C.CList a
-- rotateRWhile p lst
--   | C.isEmpty lst = lst
--   | otherwise     = go lst
--     where
--       go xs = let cur = focus xs
--                   xs' = C.rotR xs
--                   nxt = focus' xs'
--               in if p cur nxt then go xs' else xs
