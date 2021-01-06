{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
module Data.Geometry.BezierSpline(
    BezierSpline (BezierSpline)
  , controlPoints
  , fromPointSeq
  , endPoints
  , Data.Geometry.BezierSpline.reverse

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

  , pattern Bezier2, pattern Bezier3
  ) where

import           Control.Lens hiding (Empty)
import qualified Data.Foldable as F
import           Data.Geometry.Point
import           Data.Geometry.Properties
import           Data.Geometry.Transformation
import           Data.Geometry.Vector hiding (init)
import           Data.Geometry.Box.Internal
import           Data.Ext
import           Data.List (sort)
import qualified Data.List.NonEmpty as NonEmpty
import           Data.LSeq (LSeq)
import qualified Data.LSeq as LSeq
import           Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq
import           Data.Traversable (fmapDefault,foldMapDefault)
import           GHC.TypeNats
import qualified Test.QuickCheck as QC

import Data.Geometry.LineSegment hiding (endPoints)
import Data.Geometry.Ball
import Data.Geometry.Polygon
import Data.Geometry.Polygon.Convex hiding (merge)
import Algorithms.Geometry.ConvexHull.GrahamScan
import Algorithms.Geometry.SmallestEnclosingBall.RIC
import Algorithms.Geometry.SmallestEnclosingBall.Types

import Debug.Trace

--------------------------------------------------------------------------------

-- | Datatype representing a Bezier curve of degree \(n\) in \(d\)-dimensional space.
newtype BezierSpline n d r = BezierSpline { _controlPoints :: LSeq n (Point d r) }
makeLenses ''BezierSpline

-- | Quadratic Bezier Spline
pattern Bezier2      :: Point d r -> Point d r -> Point d r -> BezierSpline 2 d r
pattern Bezier2 p q r <- ((F.toList . LSeq.take 3 . _controlPoints) -> [p,q,r])
  where
    Bezier2 p q r = fromPointSeq . Seq.fromList $ [p,q,r]
{-# COMPLETE Bezier2 #-}

-- | Cubic Bezier Spline
pattern Bezier3         :: Point d r -> Point d r -> Point d r -> Point d r -> BezierSpline 3 d r
pattern Bezier3 p q r s <- ((F.toList . LSeq.take 4 . _controlPoints) -> [p,q,r,s])
  where
    Bezier3 p q r s = fromPointSeq . Seq.fromList $ [p,q,r,s]
{-# COMPLETE Bezier3 #-}

deriving instance (Arity d, Eq r) => Eq (BezierSpline n d r)

type instance Dimension (BezierSpline n d r) = d
type instance NumType   (BezierSpline n d r) = r


instance (Arity n, Arity d, QC.Arbitrary r) => QC.Arbitrary (BezierSpline n d r) where
  arbitrary = fromPointSeq . Seq.fromList <$> QC.vector (fromIntegral . (1+) . natVal $ C @n)

-- | Constructs the Bezier Spline from a given sequence of points.
fromPointSeq :: Seq (Point d r) -> BezierSpline n d r
fromPointSeq = BezierSpline . LSeq.promise . LSeq.fromSeq


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

-- | Reverse a BezierSpline
reverse :: (Arity d, Ord r, Num r, Show r) => BezierSpline n d r -> BezierSpline n d r
reverse = controlPoints %~ (LSeq.promise . LSeq.fromSeq . Seq.fromList . Prelude.reverse . F.toList)


-- | Evaluate a BezierSpline curve at time t in [0, 1]
--
-- pre: \(t \in [0,1]\)
evaluate    :: (KnownNat n, Arity d, Ord r, Num r, Show r) => BezierSpline n d r -> r -> Point d r
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

-- | Extract the endpoints.
endPoints :: forall n d r. (KnownNat n, Arity d, Ord r, Num r, Show r) => BezierSpline n d r -> (Point d r, Point d r)
endPoints b = let n = fromIntegral $ natVal (C @n)
              in (b^?!controlPoints.ix 0, b^?!controlPoints.ix n)

-- | Restrict a Bezier curve to th,e piece between parameters t < u in [0, 1].
subBezier     :: (KnownNat n, Arity d, Ord r, Num r, Show r)
              => r -> r -> BezierSpline n d r -> BezierSpline n d r
subBezier t u = fst . split u . snd . split t

-- | Split a Bezier curve at time t in [0, 1] into two pieces.
split :: forall n d r. (KnownNat n, Arity d, Ord r, Num r, Show r)
      => r -> BezierSpline n d r -> (BezierSpline n d r, BezierSpline n d r)
split t b | t < 0 = error $ "split: " ++ show t ++ " < 0"
          | t > 1 = error $ "split: " ++ show t ++ " > 1"
          | otherwise = rawSplit t b

-- | Extend a Bezier curve to a parameter value t outside the interval [0,1].
--   For t < 0, returns a Bezier representation of the section of the underlying curve
--   from parameter value t until paramater value 0. For t > 1, the same from 1 to t.
extension :: forall n d r. (KnownNat n, Arity d, Ord r, Num r, Show r)
      => r -> BezierSpline n d r -> BezierSpline n d r
extension t b | t > 0 && t < 1 = error $ "extension: 0 < " ++ show t ++ " < 1"
              | t <= 0 = fst $ rawSplit t b
              | t >= 1 = snd $ rawSplit t b

-- | Extend a Bezier curve to a parameter value t outside the interval [0,1].
--   For t < 0, returns a Bezier representation of the section of the underlying curve
--   from parameter value t until paramater value 1. For t > 1, the same from 0 to t.
extend :: forall n d r. (KnownNat n, Arity d, Ord r, Num r, Show r)
      => r -> BezierSpline n d r -> BezierSpline n d r
extend t b | t > 0 && t < 1 = error $ "extend: 0 < " ++ show t ++ " < 1"
           | t <= 0 = snd $ rawSplit t b
           | t >= 1 = fst $ rawSplit t b
 
-- | Split without parameter check. If t outside [0,1], will actually extend the curve
--   rather than split it.
rawSplit :: forall n d r. (KnownNat n, Arity d, Ord r, Num r, Show r)
      => r -> BezierSpline n d r -> (BezierSpline n d r, BezierSpline n d r)
rawSplit t b = let n  = fromIntegral $ natVal (C @n)
                   ps = collect t $ b^.controlPoints
               in ( fromPointSeq . Seq.take (n + 1) $ ps
                  , fromPointSeq . Seq.drop (n + 0) $ ps
                  )


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
splitMany :: forall n d r. (KnownNat n, Arity d, Ord r, Fractional r, Show r) => [r] -> BezierSpline n d r -> [BezierSpline n d r]
splitMany ts b = splitManySorted (sort $ map (restrict "splitMany" 0 1) ts) b
  where splitManySorted []       b = [b]
        splitManySorted (t : ts) b = (fst $ split t b) : splitManySorted (map (rescale t) ts) (snd $ split t b)
        rescale :: r -> r -> r
        rescale 1 u = 1
        rescale t u = (u - t) / (1 - t)


-- | Extend a Bezier curve to a point not on the curve, but on / close to the extended
--   underlying curve.
growTo :: (KnownNat n, Arity d, Ord r, Fractional r, Show r) => Point d r -> BezierSpline n d r -> BezierSpline n d r
growTo p b = 
  let t = extendedParameterOf b p 
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



-- | Merge two Bezier pieces. Assumes they can be merged into a single piece of the same degree
--   (as would e.g. be the case for the result of a 'split' operation).
--   Does not test whether this is the case!
merge :: (KnownNat n, Arity d, Ord r, Fractional r, Show r) => BezierSpline n d r -> BezierSpline n d r -> BezierSpline n d r
merge b1 b2 = let (p1, q1) = endPoints b1
                  (p2, q2) = endPoints b2
                  result | q1 /= p2 = error "merge: something is wrong, maybe need to flip one of the curves?"
                         | otherwise = snapEndpoints p1 q2 $ growTo p1 b2
              in result

-- need distance function between polyBeziers...


-- | Approximate Bezier curve by Polyline with given resolution.
approximate :: (KnownNat n, Arity d, Ord r, Fractional r, Show r)
            => r -> BezierSpline n d r -> [Point d r]
approximate r b | flat r b  = [fst $ endPoints b, snd $ endPoints b]
                | otherwise = let (b1, b2) = split 0.5 b
                              in approximate r b1 ++ tail (approximate r b2)

-- | Test whether a Bezier curve can be approximated by a single line segment,
--   given the resolution parameter.
flat :: (KnownNat n, Arity d, Ord r, Fractional r, Show r) => r -> BezierSpline n d r -> Bool
flat r b = let p = fst $ endPoints b
               q = snd $ endPoints b
               s = ClosedLineSegment (p :+ ()) (q :+ ())
               e t = sqDistanceToSeg (evaluate b t) s < r ^ 2
           in qdA p q < r ^ 2 || all e [0.2, 0.4, 0.6, 0.8]

-- general d depends on convex hull
-- parameterOf :: (Arity d, Ord r, Fractional r, Show r) => BezierSpline n d r -> Point d r -> r
-- | Given a point on (or close to) a Bezier curve, return the corresponding parameter value.
--   (For points far away from the curve, the function will return the parameter value of
--   an approximate locally closest point to the input point.)
parameterOf :: (KnownNat n, Ord r, RealFrac r, Show r) => BezierSpline n 2 r -> Point 2 r -> r
parameterOf b p | closeEnough p $ fst $ endPoints b = 0
                | closeEnough p $ snd $ endPoints b = 1
                | otherwise = parameterInterior b p

-- | Given a point on (or close to) the extension of a Bezier curve, return the corresponding 
--   parameter value, which might also be smaller than 0 or larger than 1.
--   (For points far away from the curve, the function will return the parameter value of
--   an approximate locally closest point to the input point.)
--   This implementation is not robust: might return a locally closest point on the curve 
--   even though the point lies on another part of the curve. For points on the actual
--   curve, use parameterOf instead.
extendedParameterOf      :: (Arity d, KnownNat n, Ord r, Fractional r, Show r) => BezierSpline n d r -> Point d r -> r
extendedParameterOf b p | p == fst (endPoints b) = 0
                        | p == snd (endPoints b) = 1
                        | otherwise = binarySearch (qdA p . evaluate b) (-100) 100


-- parameterInterior is slow, look into algebraic solution

-- general d depends on convex hull
-- parameterInterior :: (Arity d, Ord r, Fractional r, Show r) => BezierSpline n d r -> Point d r -> r
parameterInterior :: (KnownNat n, Ord r, RealFrac r, Show r) => BezierSpline n 2 r -> Point 2 r -> r
parameterInterior b p | sqrad (F.toList $ view controlPoints b) < treshold^2 = 0.5
                      | otherwise =
  let (b1, b2) = split 0.5 b
      recurse1 =       0.5 * parameterInterior b1 p
      recurse2 = 0.5 + 0.5 * parameterInterior b2 p
      chb1     = _simplePolygon $ convexHullB b1
      chb2     = _simplePolygon $ convexHullB b2
      in1      = insidePolygon p chb1 
      in2      = insidePolygon p chb2 
      result |     in1 &&     in2 = betterFit b p recurse1 recurse2
             |     in2 && not in2 = recurse1
             | not in2 &&     in2 = recurse2
             | sqDistanceToPolygon p chb1 < sqDistanceToPolygon p chb2 = recurse1
             | otherwise                                               = recurse2
  in result
  where treshold = 0.01

betterFit :: (KnownNat n, Arity d, Ord r, Fractional r, Show r) => BezierSpline n d r -> Point d r -> r -> r -> r
betterFit b p t u = 
  let q = evaluate b t 
      r = evaluate b u 
  in if qdA q p < qdA r p then t else u

sqDistanceToPolygon :: (Ord r, Fractional r, Show r) => Point 2 r -> SimplePolygon p r -> r
sqDistanceToPolygon point poly = minimum $ map (sqDistanceToSeg point) $ listEdges poly

-- | This function tests whether a value lies within bounds of a given interval.
--   If not, graciously continues with value snapped to interval.
--   This should never happen, but apparently it sometimes does?
restrict :: (Ord r, Show r) => String -> r -> r -> r -> r
restrict f l r x | l > r = error $ f ++ ": restrict: [" ++ show l ++ ", " ++ show r ++ "] is not an interval"
                 | x < l = trace (f ++ ": restricting " ++ show x ++ " to [" ++ show l ++ ", " ++ show r ++ "]") l
                 | x > r = trace (f ++ ": restricting " ++ show x ++ " to [" ++ show l ++ ", " ++ show r ++ "]") r
                 | otherwise = x


binarySearch                                    :: (Ord r, Fractional r, Show r) => (r -> r) -> r -> r -> r
binarySearch f l r | abs (f l - f r) < treshold = restrict "binarySearch" l r $ m
                   | derivative f m  > 0        = restrict "binarySearch" l r $ binarySearch f l m
                   | otherwise                  = restrict "binarySearch" l r $ binarySearch f m r
  where m = (l + r) / 2
        treshold = 0.000001

derivative     :: Fractional r => (r -> r) -> r -> r
derivative f x = (f (x + delta) - f x) / delta
  where delta = 0.0000001

-- | Snap a point close to a Bezier curve to the curve.
snap   :: (KnownNat n, Ord r, RealFrac r, Show r) => BezierSpline n 2 r -> Point 2 r -> Point 2 r
snap b = evaluate b . parameterOf b








-- | Compute the convex hull of a 2-dimensional Bezier curve.
--   Should also work in any dimension, but convex hull is not yet implemented.
convexHullB :: (KnownNat n, Ord r, Fractional r) => BezierSpline n 2 r -> ConvexPolygon () r
convexHullB b = convexHull $ NonEmpty.fromList $ map (:+ ()) $ F.toList $ _controlPoints b

-- | Given two Bezier curves, list all intersection points.
--   Not exact, since for degree >= 3 there is no closed form.
--   (In principle, this algorithm works in any dimension
--   but this requires convexHull, area/volume, and intersect.)
intersectB :: (KnownNat n, Ord r, RealFrac r, Show r) => BezierSpline n 2 r -> BezierSpline n 2 r -> [Point 2 r]
intersectB a b | a == b    = [fst $ endPoints b, snd $ endPoints b] -- should really return the whole curve
               | otherwise = let [a1, a2, a3, a4] = F.toList $ _controlPoints a
                                 [b1, b2, b3, b4] = F.toList $ _controlPoints b
                             in    intersectPointsPoints [a1, a4] [b1, b4]
                                ++ intersectPointsInterior [a1, a4] b
                                ++ intersectPointsInterior [b1, b4] a
                                ++ intersectInteriorInterior [a1, a4, b1, b4] a b


closeEnough :: (Arity d, Ord r, Fractional r) => Point d r -> Point d r -> Bool
closeEnough p q = qdA p q < treshold ^ 2
  where treshold = 0.01

intersectPointsPoints :: (Ord r, Fractional r) => [Point 2 r] -> [Point 2 r] -> [Point 2 r]
intersectPointsPoints ps qs = filter (\q -> any (closeEnough q) ps) qs
  
intersectPointsInterior :: (KnownNat n, Ord r, RealFrac r, Show r) => [Point 2 r] -> BezierSpline n 2 r -> [Point 2 r]
intersectPointsInterior ps b = let [b1, b2, b3, b4] = F.toList $ _controlPoints b
                                   nearc p = closeEnough (snap b p) p
                                   near1 = closeEnough b1
                                   near4 = closeEnough b4
                               in filter (\p -> nearc p && not (near1 p) && not (near4 p)) ps
    

intersectInteriorInterior :: (KnownNat n, Ord r, RealFrac r, Show r) => [Point 2 r] -> BezierSpline n 2 r -> BezierSpline n 2 r -> [Point 2 r]
intersectInteriorInterior forbidden a b = 
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
             | any (closeEnough approx) forbidden = []
             | otherwise                          = [approx]
      recurse = intersectInteriorInterior forbidden a1 b1 
             ++ intersectInteriorInterior forbidden a1 b2 
             ++ intersectInteriorInterior forbidden a2 b1 
             ++ intersectInteriorInterior forbidden a2 b2
  in if done then result else recurse
  where treshold = 0.001

sqrad :: (Ord r, RealFrac r, Show r) => [Point 2 r] -> r
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

-- | Cut a Bezier curve into $x_i$-monotone pieces.
--   Can only be solved exactly for degree 4 or smaller.
--   Only gives rational result for degree 2 or smaller.
--   Currentlly implemented for degree 3.
splitMonotone :: (Arity d, Ord r, Enum r, Floating r, Show r) => Int -> BezierSpline 3 d r -> [BezierSpline 3 d r]
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
locallyExtremalParameters :: (Arity d, Ord r, Enum r, Floating r, Show r) => Int -> BezierSpline 3 d r -> [r]
locallyExtremalParameters i curve = 
  let [x1, x2, x3, x4] = map (view $ unsafeCoord i) $ F.toList $ _controlPoints curve
      a = 3 * x4 -  9 * x3 + 9 * x2 - 3 * x1
      b = 6 * x1 - 12 * x2 + 6 * x3
      c = 3 * x2 -  3 * x1
  in filter (\i -> 0 <= i && i <= 1) $ solveQuadraticEquation a b c


-- | Solve equation of the form ax^2 + bx + c = 0.
--   If there are multiple solutions, report in ascending order.
--   Attempt at a somewhat robust implementation.
solveQuadraticEquation :: (Ord r, Enum r, Floating r, Show r) => r -> r -> r -> [r]
solveQuadraticEquation 0 0 0 = [0..] -- error "infinite solutions"
solveQuadraticEquation a 0 0 = [0]
solveQuadraticEquation 0 b 0 = [0]
solveQuadraticEquation 0 0 c = []
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



-- | Subdivide a curve based on a sequence of points.
--   Assumes these points are all supposed to lie on the curve, and
--   snaps endpoints of pieces to these points.
--   (higher dimensions would work, but depends on convex hull)
splitByPoints :: (KnownNat n, Ord r, RealFrac r, Show r) => [Point 2 r] -> BezierSpline n 2 r -> [BezierSpline n 2 r]
splitByPoints points curve = 
  let a      = fst $ endPoints curve
      b      = snd $ endPoints curve
      intern = filter (\p -> p /= a && p /= b) points
      times  = map (parameterOf curve) intern
      tipos  = sort $ zip times intern
      pieces = splitMany (map fst tipos) curve
      stapts = [a] ++ map snd tipos
      endpts = map snd tipos ++ [b]
  in zipWith3 snapEndpoints stapts endpts pieces

snapEndpoints :: (KnownNat n, Arity d, Ord r, Fractional r, Show r) => Point d r -> Point d r -> BezierSpline n d r -> BezierSpline n d r
snapEndpoints p q curve = 
  let points = F.toList $ _controlPoints curve
      middle = tail $ init $ points
      new    = [p] ++ middle ++ [q]
  in  fromPointSeq $ Seq.fromList new 















