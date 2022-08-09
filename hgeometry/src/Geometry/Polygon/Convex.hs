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
  ( convexPolygon
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
import           Control.Lens
import           Control.Monad.Random
import           Control.Monad.ST
import           Control.Monad.State
import           Data.Coerce
import           Data.Ext
import qualified Data.Foldable as F
import           Data.Foldable.Util
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
import           Geometry.LineSegment.Boxed
import           Geometry.LineSegment.Class
import           Geometry.Point
-- import           Geometry.Point.WithExtra
import           Geometry.Polygon.Class
import           Geometry.Polygon.Convex.Class
import           Geometry.Polygon.Convex.New
import           Geometry.Polygon.Convex.Tangents
import           Geometry.Polygon.Extremes (cmpExtreme)
import           Geometry.Polygon.Simple
import           Geometry.Properties
import           Geometry.Transformation
import           Geometry.Triangle
import           Geometry.Vector
import           Data.List.Alternating (withNeighbours)
import           Data.Vector.NonEmpty.Util ()

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

-- Melkman's algorithm: http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.512.9681&rep=rep1&type=pdf

-- | \( O(n) \) Convex hull of a simple polygon.
--
--   For algorithmic details see: <https://en.wikipedia.org/wiki/Convex_hull_of_a_simple_polygon>
convexPolygon   :: forall polygon point r. (Ord r, Num r, Polygon_ polygon point r)
                => polygon point r -> ConvexPolygon point r
convexPolygon p = uncheckedFromCCWPoints $ NE.unsafeCreate $ runM (numVertices p) $
    findStartingPoint 2
  where

    -- Find the first spot where 0,n-1,n is not colinear.
    findStartingPoint :: Int -> M s (point 2 r) ()
    findStartingPoint nth = do
      let vPrev = NE.unsafeIndex vs (nth-1)
          vNth = NE.unsafeIndex vs nth
      case ccw v1 vPrev vNth of
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
    vs = NE.unsafeFromList $ p^..outerBoundary -- FIXME
    build v = do
      botTurn <- ccw <$> pure v     <*> dequeBottom 0 <*> dequeBottom 1
      topTurn <- ccw <$> dequeTop 1 <*> dequeTop 0    <*> pure v
      when (botTurn == CW || topTurn == CW) $ do
        backtrackTop v; dequePush v
        backtrackBot v; dequeInsert v
    backtrackTop v = do
      turn <- ccw <$> dequeTop 1 <*> dequeTop 0 <*> pure v
      unless (turn == CCW) $ do
        dequePop
        backtrackTop v
    backtrackBot v = do
      turn <- ccw <$> pure v <*> dequeBottom 0 <*> dequeBottom 1
      unless (turn == CCW) $ do
        dequeRemove
        backtrackBot v

-- withNeighbours :: Getting All (simplePolygon point r) (point 2 r, (point 2 r, point 2 r))



-- | \( O(n) \) Check if a polygon is strictly convex.
isConvex :: (Ord r, Num r, SimplePolygon_ simplePolygon point r) => simplePolygon point r -> Bool
isConvex = allOf outerBoundaryWithNeighbours (\(u,(v,w)) -> ccw u v w == CCW)

-- | \( O(n) \) Verify that a convex polygon is strictly convex.
verifyConvex :: (Ord r, Num r, Point_ point 2 r) => ConvexPolygon point r -> Bool
verifyConvex = isConvex

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
extremes     :: (Num r, Ord r, ConvexPolygon_ convexPolygon point r)
             => Vector 2 r -> convexPolygon point r
             -> (point 2 r, point 2 r)
extremes u p = (maxInDirection ((-1) *^ u) p, maxInDirection u p)

-- | Finds the extreme maximum point in the given direction. Based on
-- http://geomalgorithms.com/a14-_extreme_pts.html
--
--
-- pre: The input polygon is strictly convex.
--
-- running time: \(O(\log n)\)
maxInDirection   :: (Num r, Ord r, ConvexPolygon_ convexPolygon point r )
                 => Vector 2 r -> convexPolygon point r -> point 2 r
maxInDirection u = findMaxWith (cmpExtreme u)







-- * Merging Two convex Hulls


-- | Rotating Right <-> rotate clockwise
--
-- Merging two convex hulls, based on the paper:
--
-- Two Algorithms for Constructing a Delaunay Triangulation
-- Lee and Schachter
-- International Journal of Computer and Information Sciences, Vol 9, No. 3, 1980
--
-- : (combined hull, lower tangent that was added, upper tangent that was
-- added)
--
-- pre: - lp and rp are disjoint, and there is a vertical line separating
--        the two polygons.
--      - The vertices of the polygons are given in clockwise order
--
-- Running time: O(n+m), where n and m are the sizes of the two polygons respectively
merge       :: (Num r, Ord r, ConvexPolygon_ convexPolygon point r)
            => convexPolygon point r  -> convexPolygon point r
            -> (convexPolygon point r, ClosedLineSegment 2 point r, ClosedLineSegment 2 point r)
merge lp rp = ( uncheckedFromCCWPoints $ r' ++ l', lt, ut)
  where
    lt@(ClosedLineSegment a b) = lowerTangent lp rp
    ut@(ClosedLineSegment c d) = upperTangent lp rp

    takeUntil p xs = let (xs',x:_) = break p xs in xs' ++ [x]
    takeAndRotate x y
      = takeUntil (\p -> fromGenericPoint p == fromGenericPoint @Point x)
      . F.toList . CV.rightElements
      . rotateTo' y . getVertices

    r' = takeAndRotate b d rp
    l' = takeAndRotate c a lp


-- | get the vertices of a polygon as a circularvector
getVertices   :: HasOuterBoundary polygon => polygon -> CircularVector (Vertex polygon)
getVertices p = CV.unsafeFromListN (numVertices p) (p^..outerBoundary)

rotateTo' (fromGenericPoint @Point -> x) = fromJust
                                         . CV.findRotateTo (\p -> fromGenericPoint p == x)

--------------------------------------------------------------------------------

-- | Computes the Minkowski sum of the two input polygons with $n$ and $m$
-- vertices respectively.
--
-- pre: input polygons are in CCW order.
--
-- running time: \(O(n+m)\).
minkowskiSum     :: forall convexPolygon point r. ( Ord r, Num r
                    , ConvexPolygon_ convexPolygon point r
                    )
                 => convexPolygon point r -> convexPolygon point r -> convexPolygon point r
minkowskiSum p q = uncheckedFromCCWPoints $ merge' (f p) (f q)
  where
    f p' = let (v:xs) = F.toList . bottomMost . getVertices $ p'
           in v:xs++[v]

    (.+.)   :: point 2 r -> point 2 r -> point 2 r
    v .+. w = v .+^ (w^.asVector)

    cmpAngle            :: point 2 r -> point 2 r -> point 2 r -> point 2 r -> Ordering
    cmpAngle v v' w w' =
      ccwCmpAround origin (Point $ v' .-. v) (Point $ w' .-. w)

    merge' [_]       [_]       = []
    merge' vs@[v]    (w:ws)    = v .+. w : merge' vs ws
    merge' (v:vs)    ws@[w]    = v .+. w : merge' vs ws
    merge' (v:v':vs) (w:w':ws) = v .+. w :
      case cmpAngle v v' w w' of
        LT -> merge' (v':vs)   (w:w':ws)
        GT -> merge' (v:v':vs) (w':ws)
        EQ -> merge' (v':vs)   (w':ws)
    merge' _         _         = error "minkowskiSum: Should not happen"


-- | Rotate to the leftmost point (and bottommost in case of ties)
bottomMost :: (Ord r, Point_ point 2 r)
         => CircularVector (point 2 r) -> CircularVector (point 2 r)
bottomMost = CV.rotateToMinimumBy (comparing $ \p -> (p^.yCoord,p^.xCoord))

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
inConvex :: forall convexPolygon point point' r.
            (Num r, Ord r, ConvexPolygon_ convexPolygon point r, Point_ point' 2 r
            )
         => point' 2 r -> convexPolygon point r
         -> PointLocationResult
inConvex (fromGenericPoint @point -> q) poly
  | q `intersects` leftEdge  = OnBoundary
  | q `intersects` rightEdge = OnBoundary
  | otherwise                = worker 1 n
  where
    n         = numVertices poly - 1
    point0    = point 0
    leftEdge  = ClosedLineSegment point0 (point n)
    rightEdge = ClosedLineSegment point0 (point 1)
    worker a b
      | a+1 == b                        =
        if q `onSegment` ClosedLineSegment (point a) (point b)
          then OnBoundary
          else
            if inTriangle q (triangle point0 (point a) (point b)) == Outside
              then Outside
              else Inside
      | ccw point0 (point c) q == CCW = worker c b
      | otherwise                     = worker a c
      where c = (a+b) `div` 2
    point i = poly ^. outerBoundaryVertexAt i

    triangle a b c = Triangle' (fromGenericPoint a) (fromGenericPoint b) (fromGenericPoint c)


--------------------------------------------------------------------------------
-- Diameter

-- | \( O(n) \) Computes the Euclidean diameter by scanning antipodal pairs.
diameter :: (Ord r, Radical r, ConvexPolygon_ convexPolygon point r)
         => convexPolygon point r -> r
diameter = uncurry euclideanDist . diametralPair

-- | \( O(n) \)
--   Computes the Euclidean diametral pair by scanning antipodal pairs.
diametralPair   :: (Ord r, Num r, ConvexPolygon_ convexPolygon point r)
                => convexPolygon point r -> (point 2 r, point 2 r)
diametralPair p = (p^.outerBoundaryVertexAt a, p^.outerBoundaryVertexAt b)
  where
    (a,b) = diametralIndexPair p

-- | \( O(n) \)
--   Computes the Euclidean diametral pair by scanning antipodal pairs.
diametralIndexPair   :: (Ord r, Num r, ConvexPolygon_ convexPolygon point r)
                     => convexPolygon point r -> (Int, Int)
diametralIndexPair p = F.maximumBy fn $ antipodalPairs p
  where
    fn (a1,b1) (a2,b2) =
      squaredEuclideanDist (p^.outerBoundaryVertexAt a1) (p^.outerBoundaryVertexAt b1)
        `compare`
      squaredEuclideanDist (p^.outerBoundaryVertexAt a2) (p^.outerBoundaryVertexAt b2)

antipodalPairs   :: forall convexPolygon point r.
                    (Ord r, Num r, ConvexPolygon_ convexPolygon point r)
                 => convexPolygon point r -> [(Int, Int)]
antipodalPairs p = worker 0 (vectors V.! 0) 1
  where
    n = numVertices p
    worker a aElt b
      | a == n = []
      | otherwise =
        case ccw aElt origin (vectors V.! b) of
          CW -> worker a aElt (b+1)
          _  -> (a, b `mod` n) : worker (a+1) (vectors V.! (a+1)) b

    -- vectors :: Vector (point 2 r)
    vectors = V.generate n $ \i -> point (i+1) .-. point i

    point x = p^.outerBoundaryVertexAt x

--------------------------------------------------------------------------------



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
randomConvex :: RandomGen g => Int -> Int -> Rand g (ConvexPolygon Point Rational)
randomConvex n _vMax | n < 3 =
  error "Geometry.Polygon.Convex.randomConvex: At least 3 edges are required."
randomConvex n vMax = go <$> randomEdges n vMax
  where
    go = fromRandomEdges
       . coerce . sortAround origin . coerce @[Vector 2 Int] @[Point 2 Int]
    fromRandomEdges ~(v:vs) =
      let vertices  = fmap ((/ realToFrac vMax) . realToFrac) <$> scanl (.+^) (Point v) vs
          pRational = uncheckedFromCCWPoints vertices
          c         = (centroid pRational)^.asVector
      in pRational&vertices' %~ (.-^ c)
