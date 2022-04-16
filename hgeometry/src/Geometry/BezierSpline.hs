{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE UndecidableInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Geometry.BezierSpline
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--------------------------------------------------------------------------------
module Geometry.BezierSpline(
    BezierSpline (BezierSpline, Bezier2, Bezier3)
  , controlPoints
  , fromPointSeq
  , endPoints
  , Geometry.BezierSpline.reverse

  , evaluate
  , split
  , splitMany
  , splitMonotone
  , splitByPoints
  , extension
  , extend
  , growTo
  , merge
  , subBezier
  , tangent
  , approximate
  , parameterOf
  , snap
  , intersectB
  , colinear
  , quadToCubic
  ) where

import           Algorithms.Geometry.ConvexHull.GrahamScan
import           Algorithms.Geometry.SmallestEnclosingBall.RIC
import           Algorithms.Geometry.SmallestEnclosingBall.Types
import           Control.Lens hiding (Empty)
import           Data.Ext
import qualified Data.Foldable as F
import           Geometry.Ball
import           Geometry.Box.Internal
import           Geometry.Line
import           Geometry.LineSegment hiding (endPoints)
import           Geometry.Point
import           Geometry.PolyLine (PolyLine(..))
import           Geometry.Polygon
import           Geometry.Polygon.Convex hiding (merge)
import           Geometry.Properties
import           Geometry.Transformation
import           Geometry.Vector hiding (init)
import           Data.LSeq (LSeq)
import qualified Data.LSeq as LSeq
import           Data.List (sort)
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq
import           Data.Traversable (fmapDefault,foldMapDefault)
import           GHC.TypeNats
import qualified Test.QuickCheck as QC

-- import Debug.Trace

--------------------------------------------------------------------------------

-- | Datatype representing a Bezier curve of degree \(n\) in \(d\)-dimensional space.
newtype BezierSpline n d r = BezierSpline { _controlPoints :: LSeq (1+n) (Point d r) }
-- makeLenses ''BezierSpline

-- | Bezier control points. With n degrees, there are n+1 control points.
controlPoints :: Iso (BezierSpline n1 d1 r1)     (BezierSpline n2 d2 r2)
                     (LSeq (1+n1) (Point d1 r1)) (LSeq (1+n2) (Point d2 r2))
controlPoints = iso _controlPoints BezierSpline

-- | Quadratic Bezier Spline
pattern Bezier2      :: Point d r -> Point d r -> Point d r -> BezierSpline 2 d r
pattern Bezier2 p q r <- (F.toList . LSeq.take 3 . _controlPoints -> [p,q,r])
  where
    Bezier2 p q r = fromPointSeq . Seq.fromList $ [p,q,r]
{-# COMPLETE Bezier2 #-}

-- | Cubic Bezier Spline
pattern Bezier3         :: Point d r -> Point d r -> Point d r -> Point d r -> BezierSpline 3 d r
pattern Bezier3 p q r s <- (F.toList . LSeq.take 4 . _controlPoints -> [p,q,r,s])
  where
    Bezier3 p q r s = fromPointSeq . Seq.fromList $ [p,q,r,s]
{-# COMPLETE Bezier3 #-}

-- | Constructs the Bezier Spline from a given sequence of points.
fromPointSeq :: Seq (Point d r) -> BezierSpline n d r
fromPointSeq = BezierSpline . LSeq.promise . LSeq.fromSeq


deriving instance (Arity d, Eq r) => Eq (BezierSpline n d r)

type instance Dimension (BezierSpline n d r) = d
type instance NumType   (BezierSpline n d r) = r

instance (Arity n, Arity d, QC.Arbitrary r) => QC.Arbitrary (BezierSpline n d r) where
  arbitrary = fromPointSeq . Seq.fromList <$> QC.vector (fromIntegral . (1+) . natVal $ C @n)

{-
instance (Arity n, Arity d, QC.Arbitrary r, Ord r) => QC.Arbitrary (BezierSpline n d r) where
  arbitrary = fromPointSeq . Seq.fromList <$> allDifferent (fromIntegral . (1+) . natVal $ C @n)

-- | Generates a set of unique items.
allDifferent   :: (Ord a, QC.Arbitrary  a) => Int -> QC.Gen [a]
allDifferent n = take n . Set.toList . go maxattempts mempty <$> QC.infiniteList
  where
    maxattempts = 100
    go 0 s _                        = s -- too many attempts
    go t s (x:xs) | Set.size s == n = s
                  | otherwise       = go (t-1) (Set.insert x s) xs
-}

instance (Arity d, Show r) => Show (BezierSpline n d r) where
  show (BezierSpline ps) =
    mconcat [ "BezierSpline", show $ length ps - 1, " ", show (F.toList ps) ]

instance Arity d => Functor (BezierSpline n d) where
  fmap = fmapDefault

instance Arity d => Foldable (BezierSpline n d) where
  foldMap = foldMapDefault

instance Arity d => Traversable (BezierSpline n d) where
  traverse f (BezierSpline ps) = BezierSpline <$> traverse (traverse f) ps

instance (Fractional r, Arity d, Arity (d + 1), Arity n)
          => IsTransformable (BezierSpline n d r) where
  transformBy = transformPointFunctor

instance PointFunctor (BezierSpline n d) where
  pmap f = over controlPoints (fmap f)

--------------------------------------------------------------------------------

-- | Convert a quadratic bezier to a cubic bezier.
quadToCubic :: Fractional r => BezierSpline 2 2 r -> BezierSpline 3 2 r
quadToCubic (Bezier2 a (Point b) c) =
  Bezier3 a (Point $ (1/3)*^ (toVec a ^+^ 2*^b)) (Point $ (1/3)*^ (2*^ b ^+^ toVec c)) c

--------------------------------------------------------------------------------

-- | Reverse a BezierSpline
reverse :: (Arity d, Ord r, Num r) => BezierSpline n d r -> BezierSpline n d r
reverse = controlPoints %~ LSeq.reverse


-- | Evaluate a BezierSpline curve at time t in [0, 1]
--
-- pre: \(t \in [0,1]\)
evaluate     :: (Arity d, Eq r, Num r) => BezierSpline n d r -> r -> Point d r
evaluate b 0 = fst $ endPoints b
evaluate b 1 = snd $ endPoints b
evaluate b t = evaluate' (b^.controlPoints.to LSeq.toSeq)
  where
    evaluate' = \case
      (p :<| Empty)  -> p
      pts@(_ :<| tl) -> let (ini :|> _) = pts in evaluate' $ Seq.zipWith blend ini tl
      _              -> error "evaluate: absurd"
    blend p q = p .+^ t *^ (q .-. p)

-- | Extract a tangent vector from the first to the second control point.
tangent   :: (Arity d, Num r, 1 <= n) => BezierSpline n d r -> Vector d r
tangent b = b^?!controlPoints.ix 1 .-. b^?!controlPoints.ix 0

-- | Return the endpoints of the Bezier spline.
endPoints   :: BezierSpline n d r -> (Point d r, Point d r)
endPoints b = let (p LSeq.:<| _) = b^.controlPoints
                  (_ LSeq.:|> q) = b^.controlPoints
              in (p,q)




-- | Restrict a Bezier curve to the piece between parameters t < u in [0, 1].
subBezier     :: (KnownNat n, Arity d, Ord r, Num r)
              => r -> r -> BezierSpline n d r -> BezierSpline n d r
subBezier t u = fst . split u . snd . split t


-- | Compute the convex hull of the control polygon of a 2-dimensional Bezier curve.
--   Should also work in any dimension, but convex hull is not yet implemented.
convexHullB :: (Ord r, Fractional r) => BezierSpline n 2 r -> ConvexPolygon () r
convexHullB = convexHull . NonEmpty.fromList . fmap ext . F.toList . _controlPoints

--------------------------------------------------------------------------------

-- | Split a Bezier curve at time t in [0, 1] into two pieces.
split                 :: forall n d r. (KnownNat n, Arity d, Ord r, Num r)
                      => r -> BezierSpline n d r -> (BezierSpline n d r, BezierSpline n d r)
split t b | t < 0     = error "split: t < 0" -- ++ show t ++ " < 0"
          | t > 1     = error "split: t > 1" -- ++ show t ++ " > 1"
          | otherwise = splitRaw t b


-- | Split without parameter check. If t outside [0,1], will actually extend the curve
--   rather than split it.
splitRaw     :: forall n d r. (KnownNat n, Arity d, Ord r, Num r)
             => r -> BezierSpline n d r -> (BezierSpline n d r, BezierSpline n d r)
splitRaw t b = let n  = fromIntegral $ natVal (C @n)
                   ps = collect t $ b^.controlPoints
               in ( fromPointSeq . Seq.take (n + 1) $ ps
                  , fromPointSeq . Seq.drop (n + 0) $ ps
                  )

-- | implementation of splitRaw
collect   :: (Arity d, Ord r, Num r) => r -> LSeq n (Point d r) -> Seq (Point d r)
collect t = go . LSeq.toSeq
  where
    go = \case
      ps@(_ :<| Empty) -> ps
      ps@(p :<| tl)    -> let (ini :|> q) = ps in (p :<| go (Seq.zipWith blend ini tl)) :|> q
      _                -> error "collect: absurd"

    blend p q = p .+^ t *^ (q .-. p)

-- | Split a Bezier curve into many pieces.
--   Todo: filter out duplicate parameter values!
splitMany :: forall n d r. (KnownNat n, Arity d, Ord r, Fractional r)
          => [r] -> BezierSpline n d r -> [BezierSpline n d r]
splitMany = splitManySorted . sort . map (restrict "splitMany" 0 1)

  where splitManySorted []       b' = [b']
        splitManySorted (t : ts) b' = let (a,c) = split t b'
                                      in a : splitManySorted (map (rescale t) ts) c
        rescale :: r -> r -> r
        rescale 1 _ = 1
        rescale t u = (u - t) / (1 - t)


-- | Cut a Bezier curve into $x_i$-monotone pieces.
--   Can only be solved exactly for degree 4 or smaller.
--   Only gives rational result for degree 2 or smaller.
--   Currentlly implemented for degree 3.
splitMonotone :: (Arity d, Ord r, Enum r, Floating r) => Int -> BezierSpline 3 d r -> [BezierSpline 3 d r]
splitMonotone i b = splitMany (locallyExtremalParameters i b) b

{-
type family RealTypeConstraint (n :: Nat) (r :: *) :: Constraint where
  RealTypeConstraint 1 r = (Fractional r)
  RealTypeConstraint 2 r = (Fractional r)
  RealTypeConstraint 3 r = (Floating r)
  RealTypeConstraint 4 r = (Floating r)
  RealTypeConstraint 5 r = (Floating r)
  RealTypeConstraint n r = TypeError ""
-}

-- | Report all parameter values at which the derivative of the $i$th coordinate is 0.
locallyExtremalParameters         :: (Arity d, Ord r, Enum r, Floating r)
                                  => Int -> BezierSpline 3 d r -> [r]
locallyExtremalParameters i curve =
  let [x1, x2, x3, x4] = map (view $ unsafeCoord i) $ F.toList $ _controlPoints curve
      a = 3 * x4 -  9 * x3 + 9 * x2 - 3 * x1
      b = 6 * x1 - 12 * x2 + 6 * x3
      c = 3 * x2 -  3 * x1
  in filter (\j -> 0 <= j && j <= 1) $ solveQuadraticEquation a b c


-- | Subdivide a curve based on a sequence of points.
--   Assumes these points are all supposed to lie on the curve, and
--   snaps endpoints of pieces to these points.
--   (higher dimensions would work, but depends on convex hull)
splitByPoints :: (KnownNat n, Ord r, RealFrac r)
              => r -> [Point 2 r] -> BezierSpline n 2 r -> [BezierSpline n 2 r]
splitByPoints treshold points curve =
  let a      = fst $ endPoints curve
      b      = snd $ endPoints curve
      intern = filter (\p -> p /= a && p /= b) points
      times  = map (parameterOf treshold curve) intern
      tipos  = sort $ zip times intern
      pieces = splitMany (map fst tipos) curve
      stapts = a : map snd tipos
      endpts = map snd tipos ++ [b]
  in zipWith3 snapEndpoints stapts endpts pieces

--------------------------------------------------------------------------------

-- | Extend a Bezier curve to a parameter value t outside the interval [0,1].
--   For t < 0, returns a Bezier representation of the section of the underlying curve
--   from parameter value t until paramater value 0. For t > 1, the same from 1 to t.
--
-- pre: t outside [0,1]
extension :: forall n d r. (KnownNat n, Arity d, Ord r, Num r)
      => r -> BezierSpline n d r -> BezierSpline n d r
extension t b | t > 0 && t < 1        = error "extension: 0 < t < 1" -- ++ show t ++ " < 1"
              | t <= 0                = fst $ splitRaw t b
              | otherwise {- t >= 1-} = snd $ splitRaw t b

-- | Extend a Bezier curve to a parameter value t outside the interval [0,1].
--   For t < 0, returns a Bezier representation of the section of the underlying curve
--   from parameter value t until paramater value 1. For t > 1, the same from 0 to t.
--
-- pre: t outside [0,1]
extend :: forall n d r. (KnownNat n, Arity d, Ord r, Num r)
      => r -> BezierSpline n d r -> BezierSpline n d r
extend t b | t > 0 && t < 1         = error "extend: 0 < t < 1" -- ++ show t ++ " < 1"
           | t <= 0                 = snd $ splitRaw t b
           | otherwise {- t >= 1 -} = fst $ splitRaw t b


-- | Extend a Bezier curve to a point not on the curve, but on / close
--   to the extended underlying curve.
growTo              :: (KnownNat n, Arity d, Ord r, Fractional r)
                    => r -> Point d r -> BezierSpline n d r -> BezierSpline n d r
growTo treshold p b =
  let t = extendedParameterOf treshold b p
      r | t < 0 = extend t b
        | t > 1 = extend t b
        | otherwise = b
  in r

{-

-- | Tries to fit a degree n Bezier curve through a list of points, with error parameter eps.
--   Either returns an appropriate curve, or fails.
fit :: r -> [Point 2 r] -> Maybe (Bezier n d r)
fit eps pts

-}


--------------------------------------------------------------------------------

-- | Merge two Bezier pieces. Assumes they can be merged into a single piece of the same degree
--   (as would e.g. be the case for the result of a 'split' operation).
--   Does not test whether this is the case!
merge                :: (KnownNat n, Arity d, Ord r, Fractional r)
                     => r -> BezierSpline n d r -> BezierSpline n d r -> BezierSpline n d r
merge treshold b1 b2 = let (p1, q1) = endPoints b1
                           (p2, q2) = endPoints b2
                           result | q1 /= p2 = error "merge: something is wrong, maybe need to flip one of the curves?"
                                  | otherwise = snapEndpoints p1 q2 $ growTo treshold p1 b2
                       in result

-- need distance function between polyBeziers...


--------------------------------------------------------------------------------


-- | Approximate Bezier curve by Polyline with given resolution.  That
-- is, every point on the approximation will have distance at most res
-- to the Bezier curve.
approximate     :: (KnownNat n, Arity d, Ord r, Fractional r)
                => r -> BezierSpline n d r -> PolyLine d () r
approximate res = PolyLine . fmap ext . approximate' res

-- | implementation of approximate; returns the polyline as an LSeq
approximate'     :: (KnownNat n, Arity d, Ord r, Fractional r)
                 => r -> BezierSpline n d r -> LSeq 2 (Point d r)
approximate' res = LSeq.promise . LSeq.fromSeq . go
  where
    go b | flat res b = let (p,q) = endPoints b in Seq.fromList [p,q]
         | otherwise  = let (b1, b2) = split 0.5 b in go b1 <> Seq.drop 1 (go b2)

-- | Test whether a Bezier curve can be approximated by a single line segment,
--   given the resolution parameter.
flat :: (KnownNat n, Arity d, Ord r, Fractional r) => r -> BezierSpline n d r -> Bool
flat r b = let p = fst $ endPoints b
               q = snd $ endPoints b
               s = ClosedLineSegment (p :+ ()) (q :+ ())
               e t = squaredEuclideanDistTo (evaluate b t) s < r ^ 2
           in qdA p q < r ^ 2 || all e [0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9]

-- seems this is now covered by approximate
--
--
-- -- | Approximate curve as line segments where no point on the curve is further away
-- --   from the nearest line segment than the given tolerance.
-- lineApproximate :: (Ord r, Fractional r) => r -> BezierSpline 3 2 r -> [Point 2 r]
-- lineApproximate eps bezier
--   | colinear eps bezier =
--     [ bezier^.controlPoints.to LSeq.head
--     , bezier^.controlPoints.to LSeq.last ]
--   | otherwise =
--     let (b1, b2) = split 0.5 bezier
--     in lineApproximate eps b1 ++ tail (lineApproximate eps b2)

-- If both control points are on the same side of the straight line from the start and end
-- points then the curve is guaranteed to be within 3/4 of the distance from the straight line
-- to the furthest control point.
-- Otherwise, if the control points are on either side of the straight line, the curve is
-- guaranteed to be within 4/9 of the maximum distance from the straight line to a control
-- point.
-- Also: 3/4 * sqrt(v) = sqrt (9/16 * v)
--       4/9 * sqrt(v) = sqrt (16/81 * v)
-- So: 3/4 * sqrt(v) < eps =>
--     sqrt(9/16 * v) < eps =>
--     9/16*v < eps*eps
-- | Return True if the curve is definitely completely covered by a line of thickness
--   twice the given tolerance. May return false negatives but not false positives.
colinear :: (Ord r, Fractional r) => r -> BezierSpline 3 2 r -> Bool
colinear eps (Bezier3 !a !b !c !d) = sqBound < eps*eps
  where ld = flip squaredEuclideanDistTo (lineThrough a d)
        sameSide = ccw a d b == ccw a d c
        maxDist = max (ld b) (ld c)
        sqBound
          | sameSide  = 9/16  * maxDist
          | otherwise = 16/81 * maxDist

--------------------------------------------------------------------------------

-- general d depends on convex hull
-- parameterOf :: (Arity d, Ord r, Fractional r) => BezierSpline n d r -> Point d r -> r
--
-- | Given a point on (or within distance treshold to) a Bezier curve, return the parameter value
--   of some point on the curve within distance treshold from p.
--   For points farther than treshold from the curve, the function will attempt to return the
--   parameter value of an approximate locally closest point to the input point, but no guarantees.
parameterOf :: (KnownNat n, Ord r, RealFrac r) => r -> BezierSpline n 2 r -> Point 2 r -> r
parameterOf treshold b p | closeEnough treshold p $ fst $ endPoints b = 0
                         | closeEnough treshold p $ snd $ endPoints b = 1
                         | otherwise = parameterInterior treshold b p

-- parameterInterior is slow, look into algebraic solution?

-- general d depends on convex hull
parameterInterior :: (KnownNat n, Ord r, RealFrac r) => r -> BezierSpline n 2 r -> Point 2 r -> r
parameterInterior treshold b p | sqrad (F.toList $ view controlPoints b) < (0.5 * treshold)^2 = 0.5
                               | otherwise =
  let (b1, b2) = split 0.5 b
      recurse1 =       0.5 * parameterInterior treshold b1 p
      recurse2 = 0.5 + 0.5 * parameterInterior treshold b2 p
      chb1     = _simplePolygon $ convexHullB b1
      chb2     = _simplePolygon $ convexHullB b2
      in1      = squaredEuclideanDistTo p chb1 < treshold^2
      in2      = squaredEuclideanDistTo p chb2 < treshold^2
      result |     in1 &&     in2 = betterFit b p recurse1 recurse2
             |     in2 && not in2 = recurse1
             | not in2 &&     in2 = recurse2
             | squaredEuclideanDistTo p chb1 < squaredEuclideanDistTo p chb2 = recurse1
             | otherwise                                                     = recurse2
  in result

-- | Given a point on (or close to) the extension of a Bezier curve, return the corresponding
--   parameter value, which might also be smaller than 0 or larger than 1.
--   (For points far away from the curve, the function will return the parameter value of
--   an approximate locally closest point to the input point.)
--
--   This implementation is not robust: might return a locally closest point on the curve
--   even though the point lies on another part of the curve. For points on the actual
--   curve, use parameterOf instead.
extendedParameterOf      :: (Arity d, KnownNat n, Ord r, Fractional r)
                         => r -> BezierSpline n d r -> Point d r -> r
extendedParameterOf treshold b p | p == fst (endPoints b) = 0
                                 | p == snd (endPoints b) = 1
                                 | otherwise = binarySearch treshold (qdA p . evaluate b) (-100) 100

----------------------------------------
-- * Stuff to implement parameterOf and extendedParameterOf

betterFit         :: (KnownNat n, Arity d, Ord r, Fractional r)
                  => BezierSpline n d r -> Point d r -> r -> r -> r
betterFit b p t u =
  let q = evaluate b t
      r = evaluate b u
  in if qdA q p < qdA r p then t else u

--------------------------------------------------------------------------------

-- | Given two Bezier curves, list all intersection points.
--   Not exact, since for degree >= 3 there is no closed form.
--   (In principle, this algorithm works in any dimension
--   but this requires convexHull, area/volume, and intersect.)
intersectB :: (KnownNat n, Ord r, RealFrac r) => r -> BezierSpline n 2 r -> BezierSpline n 2 r -> [Point 2 r]
intersectB treshold a b
  | a == b    = [fst $ endPoints b, snd $ endPoints b] -- should really return the whole curve
  | otherwise = let [a1, _a2, _a3, a4] = F.toList $ _controlPoints a
                    [b1, _b2, _b3, b4] = F.toList $ _controlPoints b
                in    intersectPointsPoints     treshold [a1, a4] [b1, b4]
                   ++ intersectPointsInterior   treshold [a1, a4] b
                   ++ intersectPointsInterior   treshold [b1, b4] a
                   ++ intersectInteriorInterior treshold [a1, a4, b1, b4] a b


closeEnough :: (Arity d, Ord r, Fractional r) => r -> Point d r -> Point d r -> Bool
closeEnough treshold p q = qdA p q < treshold ^ 2

intersectPointsPoints :: (Ord r, Fractional r) => r -> [Point 2 r] -> [Point 2 r] -> [Point 2 r]
intersectPointsPoints treshold ps = filter (\q -> any (closeEnough treshold q) ps)

intersectPointsInterior :: (KnownNat n, Ord r, RealFrac r) => r -> [Point 2 r] -> BezierSpline n 2 r -> [Point 2 r]
intersectPointsInterior treshold ps b =
  let [b1, _b2, _b3, b4] = F.toList $ _controlPoints b
      nearc p = closeEnough treshold (snap treshold b p) p
      near1 = closeEnough treshold b1
      near4 = closeEnough treshold b4
  in filter (\p -> nearc p && not (near1 p) && not (near4 p)) ps


intersectInteriorInterior :: (KnownNat n, Ord r, RealFrac r) => r -> [Point 2 r] -> BezierSpline n 2 r -> BezierSpline n 2 r -> [Point 2 r]
intersectInteriorInterior treshold forbidden a b =
  let cha      = _simplePolygon $ convexHullB a
      chb      = _simplePolygon $ convexHullB b
      (a1, a2) = split 0.5 a
      (b1, b2) = split 0.5 b
      points   = F.toList (view controlPoints a)
              ++ F.toList (view controlPoints b)
      approx   = average points
      done | not (cha `intersectsP` chb) = True
           | sqrad points < treshold^2   = True
           | otherwise                   = False
      result | not (cha `intersectsP` chb)        = []
             | any (closeEnough treshold approx) forbidden = []
             | otherwise                          = [approx]
      recurse = intersectInteriorInterior treshold forbidden a1 b1
             ++ intersectInteriorInterior treshold forbidden a1 b2
             ++ intersectInteriorInterior treshold forbidden a2 b1
             ++ intersectInteriorInterior treshold forbidden a2 b2
  in if done then result else recurse

sqrad :: (Ord r, RealFrac r) => [Point 2 r] -> r
sqrad points | length points < 2 = error "sqrad: not enough points"
sqrad points | otherwise =
  let rationalPoints :: [Point 2 Rational] -- smallestEnclosingDisk fails on Floats
      rationalPoints = map (traverse %~ realToFrac) points
      (a : b : cs) = map (:+ ()) rationalPoints
      diskResult   = smallestEnclosingDisk' a b cs
  in realToFrac $ view squaredRadius $ view enclosingDisk $ diskResult

average :: (Functor t, Foldable t, Arity d, Fractional r) => t (Point d r) -> Point d r
average ps = origin .+^ foldr1 (^+^) (fmap toVec ps) ^/ realToFrac (length ps)

{-
type instance IntersectionOf (BezierSpline n 2 r) (BezierSpline n 2 r) = [ NoIntersection
                                                                                   , [Point 2 r]
                                                                                   , BezierSpline n 2 r
                                                                                   ]


instance (KnownNat n, Ord r, Fractional r) => (BezierSpline n 2 r) `IsIntersectableWith` (BezierSpline n 2 r) where
  nonEmptyIntersection = defaultNonEmptyIntersection
  a `intersect` b = a `intersectB` b
-}


-- function to test whether two convex polygons intersect
-- for speed, first test bounding boxes
-- maybe would be faster to directly compare bounding boxes of points, rather than
-- call convex hull first?
intersectsP :: (Ord r, Fractional r) => SimplePolygon p r -> SimplePolygon p r -> Bool
intersectsP p q | not $ boundingBox p `intersects` boundingBox q = False
                | otherwise = or [a `intersects` b | a <- p & listEdges, b <- q & listEdges]
                           || (any (flip insidePolygon p) $ map _core $ F.toList $ polygonVertices q)
                           || (any (flip insidePolygon q) $ map _core $ F.toList $ polygonVertices p)
  -- first test bounding box?


{-

instance (Arity d, Floating r) => IsBoxable (BezierSpline 3 d r) where
  boundingBox b = foldr1 (<>) $ map (\i -> boundingBox (extremal True i b) <> boundingBox (extremal False i b)) [1 .. d]

-- | Find extremal points on curve in the $i$th dimension.
extremal :: Floating r => Bool -> Int -> BezierSpline 3 d r -> Point d r
extremal pos i b =
  let [p1, _, _, p4] = F.toList $ view controlPoints b
      ps = map evaluate $ locallyExtremalParameters i b
      candidates = [p1, p4] ++ ps
      result | pos     = maximumBy (unsafeCoord i . snd) candidates
             | not pos = minimumBy (unsafeCoord i . snd) candidates
  in result

-}


--------------------------------------------------------------------------------

snapEndpoints           :: (KnownNat n, Arity d, Ord r, Fractional r)
                        => Point d r -> Point d r -> BezierSpline n d r -> BezierSpline n d r
snapEndpoints p q curve =
  let points = F.toList $ _controlPoints curve
      middle = tail . init $ points
      new    = [p] ++ middle ++ [q]
  in  fromPointSeq $ Seq.fromList new


-- | Snap a point close to a Bezier curve to the curve.
snap   :: (KnownNat n, Ord r, RealFrac r) => r -> BezierSpline n 2 r -> Point 2 r -> Point 2 r
snap treshold b = evaluate b . parameterOf treshold b

--------------------------------------------------------------------------------
-- * Helper functions

-- | Solve equation of the form ax^2 + bx + c = 0.
--   If there are multiple solutions, report in ascending order.
--   Attempt at a somewhat robust implementation.
solveQuadraticEquation :: (Ord r, Enum r, Floating r) => r -> r -> r -> [r]
solveQuadraticEquation 0 0 0 = [0..] -- error "infinite solutions"
solveQuadraticEquation _ 0 0 = [0]
solveQuadraticEquation 0 _ 0 = [0]
solveQuadraticEquation 0 0 _ = []
solveQuadraticEquation a b 0 = sort [0, -b / a]
solveQuadraticEquation a 0 c | (-c / a) <  0 = []
                             | (-c / a) == 0 = [0]
                             | (-c / a) >  0 = [sqrt (-c / a)]
solveQuadraticEquation 0 b c = [-c / b]
solveQuadraticEquation a b c | almostzero a || almostzero (a / b) || almostzero (a / c) = solveQuadraticEquation 0 b c
solveQuadraticEquation a b c =
  let d = b^2 - 4 * a * c
      result | d == 0 = [-b / (2 * a)]
             | d >  0 = [(-b - sqrt d) / (2 * a), (-b + sqrt d) / (2 * a)]
             | otherwise = []
  in result
  -- trace ("soving equation " ++ show a ++ "x^2 + " ++ show b ++ "x + " ++ show c ++ " = 0") $ result

-- | Test whether a floating point number is close enough to zero, taking rounding errors into account.
almostzero :: (Floating r, Ord r) => r -> Bool
almostzero x = abs x < epsilon

-- | Treshold for rounding errors in almostzero test.
--   TODO: Should be different depending on the type.
epsilon :: Floating r => r
epsilon = 0.0001



-- | This function tests whether a value lies within bounds of a given interval.
--   If not, graciously continues with value snapped to interval.
--   This should never happen, but apparently it sometimes does?
restrict :: (Ord r) => String -> r -> r -> r -> r
restrict f l r x | l > r = error $ f <> ": restrict [l,r] is not an interval" --error $ f ++ ": restrict: [" ++ show l ++ ", " ++ show r ++ "] is not an interval"
                 --   | x < l = trace (f ++ ": restricting " ++ show x ++ " to [" ++ show l ++ ", " ++ show r ++ "]") l
                 --   | x > r = trace (f ++ ": restricting " ++ show x ++ " to [" ++ show l ++ ", " ++ show r ++ "]") r
                 | otherwise = x


binarySearch                                    :: (Ord r, Fractional r)
                                                => r -> (r -> r) -> r -> r -> r
binarySearch treshold f l r
    | abs (f l - f r) < treshold = restrict "binarySearch" l r   m
    | derivative f m  > 0        = restrict "binarySearch" l r $ binarySearch treshold f l m
    | otherwise                  = restrict "binarySearch" l r $ binarySearch treshold f m r
  where m = (l + r) / 2

derivative     :: Fractional r => (r -> r) -> r -> r
derivative f x = (f (x + delta) - f x) / delta
  where delta = 0.0000001
