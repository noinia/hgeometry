{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Geometry.Polygon.Convex
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Convex Polygons
--
--------------------------------------------------------------------------------
module Data.Geometry.Polygon.Convex( ConvexPolygon(..), simplePolygon
                                   , merge
                                   , lowerTangent, lowerTangent'
                                   , upperTangent, upperTangent'

                                   , extremes
                                   , maxInDirection

                                   , leftTangent, rightTangent

                                   , minkowskiSum
                                   , bottomMost
                                   ) where

import           Control.DeepSeq
import           Control.Lens hiding ((:<), (:>))
import           Data.CircularSeq (CSeq)
import qualified Data.CircularSeq as C
import           Data.Ext
import qualified Data.Foldable as F
import           Data.Function (on)
import           Data.Geometry.Box (IsBoxable(..))
import           Data.Geometry.LineSegment
import           Data.Geometry.Point
import           Data.Geometry.Polygon.Core (fromPoints, SimplePolygon, outerBoundary)
import           Data.Geometry.Polygon.Extremes(cmpExtreme)
import           Data.Geometry.Properties
import           Data.Geometry.Transformation
import           Data.Geometry.Vector
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Maybe (fromJust)
import           Data.Ord (comparing)
import           Data.Semigroup.Foldable (Foldable1(..))
import           Data.Sequence (viewl,viewr, ViewL(..), ViewR(..))
import qualified Data.Sequence as S
import           Data.Util

-- import           Data.Geometry.Ipe
-- import           Debug.Trace

--------------------------------------------------------------------------------

-- | Data Type representing a convex polygon
newtype ConvexPolygon p r = ConvexPolygon {_simplePolygon :: SimplePolygon p r }
                          deriving (Show,Eq,NFData)
makeLenses ''ConvexPolygon

instance PointFunctor (ConvexPolygon p) where
  pmap f (ConvexPolygon p) = ConvexPolygon $ pmap f p

-- | Polygons are per definition 2 dimensional
type instance Dimension (ConvexPolygon p r) = 2
type instance NumType   (ConvexPolygon p r) = r


instance Fractional r => IsTransformable (ConvexPolygon p r) where
  transformBy = transformPointFunctor

instance IsBoxable (ConvexPolygon p r) where
  boundingBox = boundingBox . _simplePolygon


-- convexPolygon   :: SimplePolygon p r -> Maybe (ConvexPolygon p r)
-- convexPolygon p = if isConvex p then Just p else Nothing

-- isConvex   :: SimplePolygon p r -> Bool
-- isConvex p = let ch = convexHull $ p^.vertices
--              in p^.vertices.size == ch^.simplePolygon.vertices.size


-- mainWith inFile outFile = do
--     ePage <- readSinglePageFile inFile
--     case ePage of
--       Left err                         -> error "" -- err
--       Right (page :: IpePage Rational) -> case page^..content.traverse._withAttrs _IpePath _asSimplePolygon.core of
--         []         -> error "No points found"
--         polies@(_:_) -> do
--            -- let out  = [asIpe drawTriangulation dt, asIpe drawTree' emst]
--            -- print $ length $ edges' dt
--            -- print $ toPlaneGraph (Proxy :: Proxy DT) dt
--            -- writeIpeFile outFile . singlePageFromContent $ out
--            -- mapM_ (print . extremesNaive (v2 1 0)) polies
--            pure $ map (flip rightTangent (Point2 80 528)) polies




-- | Finds the extreme points, minimum and maximum, in a given direction
--
-- pre: The input polygon is strictly convex.
--
-- running time: \(O(\log n)\)
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
-- running time: \(O(\log^2 n)\)
maxInDirection   :: (Num r, Ord r) => Vector 2 r -> ConvexPolygon p r -> Point 2 r :+ p
maxInDirection u = findMaxWith (cmpExtreme u)

findMaxWith       :: (Point 2 r :+ p -> Point 2 r :+ p -> Ordering)
                  -> ConvexPolygon p r -> Point 2 r :+ p
findMaxWith cmp p = findMaxStart . C.rightElements . getVertices $ p
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
-- running time: \(O(\log^2 n)\).
leftTangent        :: (Ord r, Num r) => ConvexPolygon p r -> Point 2 r -> Point 2 r :+ p
leftTangent poly q = findMaxWith (tangentCmp q) poly

--  | Given a convex polygon poly, and a point outside the polygon, find the
--  right tangent of q and the polygon, i.e. the vertex v of the convex polygon
--  s.t. the polygon lies completely to the left of the line from q to v.
--
-- running time: \(O(\log^2 n)\).
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
merge lp rp = (ConvexPolygon . fromPoints $ r' ++ l', lt, ut)
  where
    lt@(ClosedLineSegment a b) = lowerTangent lp rp
    ut@(ClosedLineSegment c d) = upperTangent lp rp

    takeUntil p xs = let (xs',x:_) = break p xs in xs' ++ [x]
    rightElems  = F.toList . C.rightElements
    takeAndRotate x y = takeUntil (coreEq x) . rightElems . rotateTo' y . getVertices

    r' = takeAndRotate b d rp
    l' = takeAndRotate c a lp


rotateTo'   :: Eq a => (a :+ b) -> CSeq (a :+ b) -> CSeq (a :+ b)
rotateTo' x = fromJust . C.findRotateTo (coreEq x)

coreEq :: Eq a => (a :+ b) -> (a :+ b) -> Bool
coreEq = (==) `on` (^.core)


--------------------------------------------------------------------------------
-- * Computing Tangents

-- | Compute the lower tangent of the two polgyons
--
--   pre: - polygons lp and rp have at least 1 vertex
--        - lp and rp are disjoint, and there is a vertical line separating
--          the two polygons.
--        - The vertices of the polygons are given in clockwise order
--
-- Running time: O(n+m), where n and m are the sizes of the two polygons respectively
lowerTangent       :: (Num r, Ord r)
                   => ConvexPolygon p r
                   -> ConvexPolygon p r
                   -> LineSegment 2 p r
lowerTangent lp rp = ClosedLineSegment l r
  where
    mkH f = NonEmpty.fromList . F.toList . f . getVertices
    lh = mkH (C.rightElements . rightMost) lp
    rh = mkH (C.leftElements  . leftMost)  rp
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
lowerTangent'       :: (Ord r, Num r, Foldable1 f)
                    => f (Point 2 r :+ p) -> f (Point 2 r :+ p)
                    -> Two ((Point 2 r :+ p) :+ [Point 2 r :+ p])
lowerTangent' l0 r0 = go (toNonEmpty l0) (toNonEmpty r0)
  where
    ne = NonEmpty.fromList
    isRight' []    _ _ = False
    isRight' (x:_) l r = ccw' l r x /= CCW

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
-- Running time: O(n+m), where n and m are the sizes of the two polygons respectively
upperTangent       :: (Num r, Ord r)
                   => ConvexPolygon p r
                   -> ConvexPolygon p r
                   -> LineSegment 2 p r
upperTangent lp rp = ClosedLineSegment l r
  where
    mkH f = NonEmpty.fromList . F.toList . f . getVertices
    lh = mkH (C.leftElements  . rightMost) lp
    rh = mkH (C.rightElements . leftMost)  rp
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
upperTangent'       :: (Ord r, Num r, Foldable1 f)
                    => f (Point 2 r :+ p) -> f (Point 2 r :+ p)
                    -> Two ((Point 2 r :+ p) :+ [Point 2 r :+ p])
upperTangent' l0 r0 = go (toNonEmpty l0) (toNonEmpty r0)
  where
    ne = NonEmpty.fromList
    isLeft' []    _ _ = False
    isLeft' (x:_) l r = ccw' l r x /= CW

    go lh@(l:|ls) rh@(r:|rs) | isLeft' rs l r = go lh      (ne rs)
                             | isLeft' ls l r = go (ne ls) rh
                             | otherwise      = Two (l :+ ls) (r :+ rs)

--------------------------------------------------------------------------------

-- | Computes the Minkowski sum of the two input polygons with $n$ and $m$
-- vertices respectively.
--
-- pre: input polygons are in CCW order.
--
-- running time: \(O(n+m)\).
minkowskiSum     :: (Ord r, Num r)
                 => ConvexPolygon p r -> ConvexPolygon q r -> ConvexPolygon (p,q) r
minkowskiSum p q = ConvexPolygon . fromPoints $ merge' (f p) (f q)
  where
    f p' = let xs@(S.viewl -> (v :< _)) = C.asSeq . bottomMost . getVertices $ p'
           in F.toList $ xs |> v
    (v :+ ve) .+. (w :+ we) = v .+^ (toVec w) :+ (ve,we)

    cmpAngle v v' w w' =
      ccwCmpAround (ext $ origin) (ext . Point $ v' .-. v) (ext . Point $ w' .-. w)

    merge' [_]       [_]       = []
    merge' vs@[v]    (w:ws)    = v .+. w : merge' vs ws
    merge' (v:vs)    ws@[w]    = v .+. w : merge' vs ws
    merge' (v:v':vs) (w:w':ws) = v .+. w :
      case cmpAngle (v^.core) (v'^.core) (w^.core) (w'^.core) of
        LT -> merge' (v':vs)   (w:w':ws)
        GT -> merge' (v:v':vs) (w':ws)
        EQ -> merge' (v':vs)   (w':ws)
    merge' _         _         = error $ "minkowskiSum: Should not happen"




--------------------------------------------------------------------------------

-- | Rotate to the rightmost point (rightmost and topmost in case of ties)
rightMost    :: Ord r => CSeq (Point 2 r :+ p) -> CSeq (Point 2 r :+ p)
rightMost xs = let m = F.maximumBy (comparing (^.core)) xs in rotateTo' m xs

-- | Rotate to the leftmost point (and bottommost in case of ties)
leftMost    :: Ord r => CSeq (Point 2 r :+ p) -> CSeq (Point 2 r :+ p)
leftMost xs = let m = F.minimumBy (comparing (^.core)) xs in rotateTo' m xs

-- | Rotate to the bottommost point (and leftmost in case of ties)
bottomMost    :: Ord r => CSeq (Point 2 r :+ p) -> CSeq (Point 2 r :+ p)
bottomMost xs = let f p = (p^.core.yCoord,p^.core.xCoord)
                    m   = F.minimumBy (comparing f) xs
                in rotateTo' m xs



-- | Helper to get the vertices of a convex polygon
getVertices :: ConvexPolygon p r -> CSeq (Point 2 r :+ p)
getVertices = view (simplePolygon.outerBoundary)

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

-- test1 :: Num r => ConvexPolygon () r
-- test1 = ConvexPolygon . fromPoints . map ext . reverse $ [origin, Point2 1 4, Point2 5 6, Point2 10 3]

-- test2 :: Num r => ConvexPolygon () r
-- test2 = ConvexPolygon . fromPoints . map ext . reverse $ [Point2 11 6, Point2 10 10, Point2 15 18, Point2 12 5]

-- testA :: Num r => ConvexPolygon () r
-- testA = ConvexPolygon . fromPoints . map ext $ [origin, Point2 5 1, Point2 2 2]

-- testB :: Num r => ConvexPolygon () r
-- testB = ConvexPolygon . fromPoints . map ext $ [origin, Point2 5 3, Point2 (-2) 2, Point2 (-2) 1]
