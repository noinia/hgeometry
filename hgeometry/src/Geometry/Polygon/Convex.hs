{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Geometry.Polygon.Convex
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Convex Polygons
--
--------------------------------------------------------------------------------
module Geometry.Polygon.Convex
  ( ConvexPolygon(..), simplePolygon
  , convexPolygon
  , isConvex, verifyConvex
  , merge
  , lowerTangent, lowerTangent'
  , upperTangent, upperTangent'

  , extremes
  , maxInDirection

  , leftTangent, rightTangent

  , minkowskiSum
  , bottomMost
  , inConvex
  , randomConvex

  , diameter
  , diametralPair
  , diametralIndexPair
  ) where

import           Control.DeepSeq (NFData)
import           Control.Lens (Iso, iso, over, view, (%~), (&), (^.))
import           Control.Monad.Random
import           Control.Monad.ST
import           Control.Monad.State
import           Data.Coerce
import           Data.Ext
import qualified Data.Foldable as F
import           Data.Function (on)
import qualified Data.IntSet as IS
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Maybe (fromJust)
import           Data.Ord (comparing)
import           Data.Radical
import           Data.Semigroup.Foldable (Foldable1 (..))
import           Data.Util
import qualified Data.Vector as V
import           Data.Vector.Circular (CircularVector)
import qualified Data.Vector.Circular as CV
import qualified Data.Vector.Circular.Util as CV
import qualified Data.Vector.Mutable as Mut
import qualified Data.Vector.NonEmpty as NE
import qualified Data.Vector.Unboxed as VU
import           Geometry.Boundary
import           Geometry.Box (IsBoxable (..))
import           Geometry.LineSegment
import           Geometry.Point
import           Geometry.Point.WithExtra
import           Geometry.Polygon.Core     (Polygon (..), SimplePolygon, centroid,
                                                 outerBoundaryVector, outerVertex, size,
                                                 unsafeFromPoints, unsafeFromVector,
                                                 unsafeOuterBoundaryVector)
import           Geometry.Polygon.Extremes (cmpExtreme)
import           Geometry.Properties
import           Geometry.Transformation
import           Geometry.Triangle
import           Geometry.Vector
-- import           Geometry.Ipe
-- import Data.Ratio
-- import Data.RealNumber.Rational
-- import Debug.Trace

--------------------------------------------------------------------------------

-- | Data Type representing a convex polygon
newtype ConvexPolygon p r = ConvexPolygon {_simplePolygon :: SimplePolygon p r }
                          deriving (Show,Eq,NFData)

-- | ConvexPolygons are isomorphic to SimplePolygons with the added constraint that they have no
--   reflex vertices.
simplePolygon :: Iso (ConvexPolygon p1 r1) (ConvexPolygon p2 r2) (SimplePolygon p1 r1) (SimplePolygon p2 r2)
simplePolygon = iso _simplePolygon ConvexPolygon

instance PointFunctor (ConvexPolygon p) where
  pmap f (ConvexPolygon p) = ConvexPolygon $ pmap f p

-- | Polygons are per definition 2 dimensional
type instance Dimension (ConvexPolygon p r) = 2
type instance NumType   (ConvexPolygon p r) = r


instance Fractional r => IsTransformable (ConvexPolygon p r) where
  transformBy = transformPointFunctor

instance IsBoxable (ConvexPolygon p r) where
  boundingBox = boundingBox . _simplePolygon



--------------------------------------------------------------------------------
-- Convex hull of simple polygon.

type M s v a = StateT (Mut.MVector s v, Int) (ST s) a

runM :: Int -> M s v () -> ST s (Mut.MVector s v)
runM s action = do
  v <- Mut.new (2*s)
  (v', f) <- execStateT action (Mut.drop s v, 0)
  return $ Mut.tail $ Mut.take f v'

dequeRemove :: M s a ()
dequeRemove = do
  modify $ \(Mut.MVector offset len arr, f) -> (Mut.MVector (offset+1) (len-1) arr, f-1)

dequeInsert :: a -> M s a ()
dequeInsert a = do
  modify $ \(Mut.MVector offset len arr, f) -> (Mut.MVector (offset-1) (len+1) arr, f+1)
  (v,_) <- get
  Mut.write v 0 a

dequePush :: a -> M s a ()
dequePush a = do
  (v, f) <- get
  Mut.write v f a
  put (v,f+1)

dequePop :: M s a ()
dequePop = do
  modify $ \(v,f) -> (v,f-1)

dequeBottom :: Int -> M s a a
dequeBottom idx = do
  (v,_) <- get
  Mut.read v idx

dequeTop :: Int -> M s a a
dequeTop idx = do
  (v,f) <- get
  Mut.read v (f-idx-1)

ccw'       :: (Ord r, Num r) => Point 2 r :+ p -> Point 2 r :+ p -> Point 2 r :+ p -> CCW
ccw' a b c = ccw (a^.core) (b^.core) (c^.core)

-- Melkman's algorithm: http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.512.9681&rep=rep1&type=pdf

-- | \( O(n) \) Convex hull of a simple polygon.
--
--   For algorithmic details see: <https://en.wikipedia.org/wiki/Convex_hull_of_a_simple_polygon>
convexPolygon :: forall t p r. (Ord r, Num r, Show r, Show p) => Polygon t p r -> ConvexPolygon p r
convexPolygon p = ConvexPolygon $ unsafeFromVector $ V.create $ runM (size p) $
    findStartingPoint 2
  where

    -- Find the first spot where 0,n-1,n is not colinear.
    findStartingPoint :: Int -> M s (Point 2 r :+ p) ()
    findStartingPoint nth = do
      let vPrev = NE.unsafeIndex vs (nth-1)
          vNth = NE.unsafeIndex vs nth
      case ccw' v1 vPrev vNth of
        CoLinear -> findStartingPoint (nth+1)
        CCW -> do
          dequePush v1 >> dequePush vPrev
          dequePush vNth; dequeInsert vNth
          V.mapM_ build (NE.drop (nth+1) vs)
        CW -> do
          dequePush vPrev >> dequePush v1
          dequePush vNth; dequeInsert vNth
          V.mapM_ build (NE.drop (nth+1) vs)

    v1 = NE.unsafeIndex vs 0
    vs = CV.vector (p^.outerBoundaryVector)
    build v = do
      botTurn <- ccw' <$> pure v     <*> dequeBottom 0 <*> dequeBottom 1
      topTurn <- ccw' <$> dequeTop 1 <*> dequeTop 0    <*> pure v
      when (botTurn == CW || topTurn == CW) $ do
        backtrackTop v; dequePush v
        backtrackBot v; dequeInsert v
    backtrackTop v = do
      turn <- ccw' <$> dequeTop 1 <*> dequeTop 0 <*> pure v
      unless (turn == CCW) $ do
        dequePop
        backtrackTop v
    backtrackBot v = do
      turn <- ccw' <$> pure v <*> dequeBottom 0 <*> dequeBottom 1
      unless (turn == CCW) $ do
        dequeRemove
        backtrackBot v







-- | \( O(n) \) Check if a polygon is strictly convex.
isConvex :: (Ord r, Num r) => SimplePolygon p r -> Bool
isConvex s =
    CV.and (CV.zipWith3 f (CV.rotateLeft 1 vs) vs (CV.rotateRight 1 vs))
  where
    f a b c = ccw' a b c == CCW
    vs = s ^. outerBoundaryVector

-- | \( O(n) \) Verify that a convex polygon is strictly convex.
verifyConvex :: (Ord r, Num r) => ConvexPolygon p r -> Bool
verifyConvex = isConvex . _simplePolygon

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
-- running time: \(O(\log n)\)
maxInDirection   :: (Num r, Ord r) => Vector 2 r -> ConvexPolygon p r -> Point 2 r :+ p
maxInDirection u = findMaxWith (\a b -> cmpExtreme u (a^.core) (b^.core))

-- FIXME: c+1 is always less than n so we don't need to use `mod` or do bounds checking.
--        Use unsafe indexing.
-- \( O(\log n) \)
findMaxWith :: (Point 2 r :+ p -> Point 2 r :+ p -> Ordering)
             -> ConvexPolygon p r -> Point 2 r :+ p
findMaxWith cmp p = CV.index v (worker 0 (F.length v))
  where
    v = p ^. simplePolygon.outerBoundaryVector
    a `icmp` b = CV.index v a `cmp` CV.index v b
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

{- Convex binary search using sequences in \( O(log^2 n) \)

findMaxWith       :: (Point 2 r :+ p -> Point 2 r :+ p -> Ordering)
                  -> ConvexPolygon p r -> Point 2 r :+ p
findMaxWith cmp = findMaxStart . S.fromList . F.toList . getVertices
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
    binSearch ac@(viewl -> a:<r) c cb = case (isUpwards a (r |> c), isUpwards c cb, a >=. c) of
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
-}

tangentCmp       :: (Num r, Ord r)
                 => Point 2 r -> Point 2 r :+ p -> Point 2 r :+ q -> Ordering
tangentCmp o p q = case ccw o (p^.core) (q^.core) of
                     CCW      -> LT -- q is left of the line from o to p
                     CoLinear -> EQ -- q is *on* the line from o to p
                     CW       -> GT -- q is right of the line from o to p


-- | Given a convex polygon poly, and a point outside the polygon, find the
--  left tangent of q and the polygon, i.e. the vertex v of the convex polygon
--  s.t. the polygon lies completely to the right of the line from q to v.
--
-- running time: \(O(\log n)\).
leftTangent        :: (Ord r, Num r) => ConvexPolygon p r -> Point 2 r -> Point 2 r :+ p
leftTangent poly q = findMaxWith (tangentCmp q) poly

-- | Given a convex polygon poly, and a point outside the polygon, find the
--  right tangent of q and the polygon, i.e. the vertex v of the convex polygon
--  s.t. the polygon lies completely to the left of the line from q to v.
--
-- running time: \(O(\log n)\).
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
merge lp rp = (ConvexPolygon . unsafeFromPoints $ r' ++ l', lt, ut)
  where
    lt@(ClosedLineSegment a b) = lowerTangent lp rp
    ut@(ClosedLineSegment c d) = upperTangent lp rp

    takeUntil p xs = let (xs',x:_) = break p xs in xs' ++ [x]
    rightElems  = F.toList . CV.rightElements
    takeAndRotate x y = takeUntil (coreEq x) . rightElems . rotateTo' y . getVertices

    r' = takeAndRotate b d rp
    l' = takeAndRotate c a lp


rotateTo'   :: Eq a => (a :+ b) -> CircularVector (a :+ b) -> CircularVector (a :+ b)
rotateTo' x = fromJust . CV.findRotateTo (coreEq x)

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
lowerTangent       :: forall p r. (Num r, Ord r)
                   => ConvexPolygon p r
                   -> ConvexPolygon p r
                   -> LineSegment 2 p r
lowerTangent lp rp = ClosedLineSegment (coerce l) (coerce r)
  where
    coerce' = coerce @(NE.NonEmptyVector (Point 2 r :+ p))
                     @(NE.NonEmptyVector (WithExtra Point p 2 r))
    lh = coerce' . CV.rightElements . rightMost . getVertices $ lp
    rh = coerce' . CV.leftElements  . leftMost  . getVertices $ rp
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
upperTangent       :: forall p r. (Num r, Ord r)
                   => ConvexPolygon p r
                   -> ConvexPolygon p r
                   -> LineSegment 2 p r
upperTangent lp rp = ClosedLineSegment (coerce l) (coerce r)
  where
    coerce' = coerce @(NE.NonEmptyVector (Point 2 r :+ p))
                     @(NE.NonEmptyVector (WithExtra Point p 2 r))
    lh = coerce' . CV.leftElements  . rightMost . getVertices $ lp
    rh = coerce' . CV.rightElements . leftMost  . getVertices $ rp
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

-- | Computes the Minkowski sum of the two input polygons with $n$ and $m$
-- vertices respectively.
--
-- pre: input polygons are in CCW order.
--
-- running time: \(O(n+m)\).
minkowskiSum     :: (Ord r, Num r)
                 => ConvexPolygon p r -> ConvexPolygon q r -> ConvexPolygon (p,q) r
minkowskiSum p q = ConvexPolygon . unsafeFromPoints $ merge' (f p) (f q)
  where
    f p' = let (v:xs) = F.toList . bottomMost . getVertices $ p'
           in v:xs++[v]
    (v :+ ve) .+. (w :+ we) = v .+^ toVec w :+ (ve,we)

    cmpAngle v v' w w' =
      ccwCmpAround origin (Point $ v' .-. v) (Point $ w' .-. w)

    merge' [_]       [_]       = []
    merge' vs@[v]    (w:ws)    = v .+. w : merge' vs ws
    merge' (v:vs)    ws@[w]    = v .+. w : merge' vs ws
    merge' (v:v':vs) (w:w':ws) = v .+. w :
      case cmpAngle (v^.core) (v'^.core) (w^.core) (w'^.core) of
        LT -> merge' (v':vs)   (w:w':ws)
        GT -> merge' (v:v':vs) (w':ws)
        EQ -> merge' (v':vs)   (w':ws)
    merge' _         _         = error "minkowskiSum: Should not happen"


--------------------------------------------------------------------------------
-- inConvex

-- 1. Check if p is on left edge or right edge.
-- 2. Do binary search:
--       Find the largest n where p is on the right of 0 to n.
-- 3. Check if p is on segment n,n+1
-- 4. Check if p is in triangle 0,n,n+1

-- | \( O(\log n) \)
--   Check if a point lies inside a convex polygon, on the boundary, or outside of the
--   convex polygon.
inConvex :: forall p r. (Num r, Ord r)
         => Point 2 r -> ConvexPolygon p r
         -> PointLocationResult
inConvex p (ConvexPolygon poly)
  | p `intersects` leftEdge  = OnBoundary
  | p `intersects` rightEdge = OnBoundary
  | otherwise                = worker 1 n
  where
    p'        = p :+ undefined
    n         = size poly - 1
    point0    = point 0
    leftEdge  = ClosedLineSegment point0 (point n)
    rightEdge = ClosedLineSegment point0 (point 1)
    worker a b
      | a+1 == b                        =
        if p `intersects` ClosedLineSegment (point a) (point b)
          then OnBoundary
          else
            if inTriangle p (Triangle point0 (point a) (point b)) == Outside
              then Outside
              else Inside
      | ccw' point0 (point c) p' == CCW = worker c b
      | otherwise                       = worker a c
      where c = (a+b) `div` 2
    point x = poly ^. outerVertex x

--------------------------------------------------------------------------------
-- Diameter

-- | \( O(n) \) Computes the Euclidean diameter by scanning antipodal pairs.
diameter :: (Ord r, Radical r) => ConvexPolygon p r -> r
diameter p = euclideanDist (a^.core) (b^.core)
  where
    (a,b) = diametralPair p

-- | \( O(n) \)
--   Computes the Euclidean diametral pair by scanning antipodal pairs.
diametralPair :: (Ord r, Num r) => ConvexPolygon p r -> (Point 2 r :+ p, Point 2 r :+ p)
diametralPair p = (p^.simplePolygon.outerVertex a, p^.simplePolygon.outerVertex b)
  where
    (a,b) = diametralIndexPair p

-- | \( O(n) \)
--   Computes the Euclidean diametral pair by scanning antipodal pairs.
diametralIndexPair :: (Ord r, Num r) => ConvexPolygon p r -> (Int, Int)
diametralIndexPair p = F.maximumBy fn $ antipodalPairs p
  where
    fn (a1,b1) (a2,b2) =
      squaredEuclideanDist (p^.simplePolygon.outerVertex a1.core) (p^.simplePolygon.outerVertex b1.core)
        `compare`
      squaredEuclideanDist (p^.simplePolygon.outerVertex a2.core) (p^.simplePolygon.outerVertex b2.core)

antipodalPairs :: forall p r. (Ord r, Num r) => ConvexPolygon p r -> [(Int, Int)]
antipodalPairs p = worker 0 (CV.index vectors 0) 1
  where
    n = size (p^.simplePolygon)
    vs = p^.simplePolygon.outerBoundaryVector

    worker a aElt b
      | a == n = []
      | otherwise =
        case ccw aElt (Point2 0 0) (CV.index vectors b) of
          CW -> worker a aElt (b+1)
          _  ->
            (a, b `mod` n) :
            worker (a+1) (CV.index vectors (a+1)) b

    vectors :: CircularVector (Point 2 r)
    vectors = CV.unsafeFromVector $ V.generate n $ \i ->
      let Point p1 = point i
          p2 = point (i+1)
      in p2 .-^ p1

    point x = CV.index vs x ^. core

--------------------------------------------------------------------------------

-- | Rotate to the rightmost point (rightmost and topmost in case of ties)
rightMost :: Ord r => CircularVector (Point 2 r :+ p) -> CircularVector (Point 2 r :+ p)
rightMost = CV.rotateToMaximumBy (comparing (^.core))

-- | Rotate to the leftmost point (and bottommost in case of ties)
leftMost :: Ord r => CircularVector (Point 2 r :+ p) -> CircularVector (Point 2 r :+ p)
leftMost = CV.rotateToMinimumBy (comparing (^.core))

-- | Rotate to the bottommost point (and leftmost in case of ties)
bottomMost :: Ord r => CircularVector (Point 2 r :+ p) -> CircularVector (Point 2 r :+ p)
bottomMost = CV.rotateToMinimumBy (comparing f)
  where
    f p = (p^.core.yCoord,p^.core.xCoord)



-- | Helper to get the vertices of a convex polygon
getVertices :: ConvexPolygon p r -> CircularVector (Point 2 r :+ p)
getVertices = view (simplePolygon.outerBoundaryVector)

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




--------------------------------------------------------------------------------
-- Random convex polygons

-- This is true for all convex polygons:
--   1. the sum of all edge vectors is (0,0). This is even true for all polygons.
--   2. edges are sorted by angle. Ie. all vertices are convex, not reflex.
--
-- So, if we can generate a set of vectors that sum to zero then we can sort them
-- and place them end-to-end and the result will be a convex polygon.
--
-- So, we need to generate N points that sum to 0. This can be done by generating
-- two sets of N points that sum to M, and the subtracting them from each other.
--
-- Generating N points that sum to M is done like this: Generate (N-1) unique points
-- between (but not including) 0 and M. Write down the distance between the points.
-- Imagine a scale from 0 to M:
--   0            M
--   |            |
-- Then we add two randomly selected points:
--   0            M
--   |  *      *  |
-- Then we look at the distance between 0 and point1, point1 and point2, and point2 to M:
--   0            M
--   |--*------*--|
--    2     6    2
-- 2+6+2 = 10 = M
--
-- Doing this again might yield [5,2,3]. Subtract them:
--     [2,   6,   2  ]
--   - [5,   2,   3  ]
--   = [2-5, 6-2, 2-3]
--   = [-3,  4,   -1 ]
-- And the sum of [-3, 4, -1] = -3+4-1 = 0.

-- O(n log n)
randomBetween :: RandomGen g => Int -> Int -> Rand g (VU.Vector Int)
randomBetween n vMax | vMax < n+1 = pure $ VU.replicate vMax 1
randomBetween n vMax = worker (n-1) IS.empty
  where
    gen from []     = [vMax-from]
    gen from (x:xs) = (x-from) : gen x xs
    worker 0 seen = pure (VU.fromList (gen 0 $ IS.elems seen))
    worker i seen = do
      v <- getRandomR (1, vMax-1)
      if IS.member v seen
        then worker i seen
        else worker (i-1) (IS.insert v seen)

randomBetweenZero :: RandomGen g => Int -> Int -> Rand g (VU.Vector Int)
randomBetweenZero n vMax = VU.zipWith (-) <$> randomBetween n vMax <*> randomBetween n vMax

randomEdges :: RandomGen g => Int -> Int -> Rand g [Vector 2 Int]
randomEdges n vMax = do
  zipWith Vector2
    <$> fmap VU.toList (randomBetweenZero n vMax)
    <*> fmap VU.toList (randomBetweenZero n vMax)

-- | \( O(n \log n) \)
--   Generate a uniformly random ConvexPolygon with @N@ vertices and a granularity of @vMax@.
randomConvex :: RandomGen g => Int -> Int -> Rand g (ConvexPolygon () Rational)
randomConvex n _vMax | n < 3 =
  error "Geometry.Polygon.Convex.randomConvex: At least 3 edges are required."
randomConvex n vMax = do
  ~(v:vs) <- coerce . sortAround origin . coerce @[Vector 2 Int] @[Point 2 Int]  <$> randomEdges n vMax
  let vertices = fmap ((/ realToFrac vMax) . realToFrac) <$> scanl (.+^) (Point v) vs
      pRational = unsafeFromPoints $ map ext vertices
      Point c = centroid pRational
      pFinal = pRational & unsafeOuterBoundaryVector %~ CV.map (over core (.-^ c))
  pure $ ConvexPolygon pFinal
