{-# LANGUAGE UndecidableInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Geometry.BezierSpline
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--------------------------------------------------------------------------------
module Data.Geometry.BezierSpline(
    BezierSpline (BezierSpline)
  , controlPoints
  , fromPointSeq

  , evaluate
  , split
  , subBezier
  , tangent
  , approximate
  , parameterOf
  , snap

  , pattern Bezier2, pattern Bezier3
  ) where

import           Control.Lens hiding (Empty)
import qualified Data.Foldable as F
import           Data.Geometry.Point
import           Data.Geometry.Properties
import           Data.Geometry.Transformation
import           Data.Geometry.Vector
import           Data.LSeq (LSeq)
import qualified Data.LSeq as LSeq
import           Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq
import           Data.Traversable (fmapDefault,foldMapDefault)
import           GHC.TypeNats
import qualified Test.QuickCheck as QC

--------------------------------------------------------------------------------

-- | Datatype representing a Bezier curve of degree \(n\) in \(d\)-dimensional space.
newtype BezierSpline n d r = BezierSpline { _controlPoints :: LSeq (1+n) (Point d r) }
-- makeLenses ''BezierSpline

-- | Bezier control points. With n degrees, there are n+1 control points.
controlPoints :: Iso (BezierSpline n1 d1 r1) (BezierSpline n2 d2 r2) (LSeq (1+n1) (Point d1 r1)) (LSeq (1+n2) (Point d2 r2))
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

-- | Evaluate a BezierSpline curve at time t in [0, 1]
--
-- pre: \(t \in [0,1]\)
evaluate    :: (Arity d, Ord r, Num r) => BezierSpline n d r -> r -> Point d r
evaluate b t = evaluate' (b^.controlPoints.to LSeq.toSeq)
  where
    evaluate' = \case
      (p :<| Empty)  -> p
      pts@(_ :<| tl) -> let (ini :|> _) = pts in evaluate' $ Seq.zipWith blend ini tl
      _              -> error "evaluate: absurd"

    blend p q = p .+^ t *^ (q .-. p)

-- | Tangent to the bezier spline at the starting point.
tangent   :: (Arity d, Num r, 1 <= n) => BezierSpline n d r -> Vector d r
tangent b = b^?!controlPoints.ix 1  .-. b^?!controlPoints.ix 0

-- | Restrict a Bezier curve to th,e piece between parameters t < u in [0, 1].
subBezier     :: (KnownNat n, Arity d, Ord r, Num r)
              => r -> r -> BezierSpline n d r -> BezierSpline n d r
subBezier t u = fst . split u . snd . split t

-- | Split a Bezier curve at time t in [0, 1] into two pieces.
split :: forall n d r. (KnownNat n, Arity d, Ord r, Num r)
      => r -> BezierSpline n d r -> (BezierSpline n d r, BezierSpline n d r)
split t b | t < 0 || t > 1 = error "Split parameter out of bounds."
          | otherwise      = let n  = fromIntegral $ natVal (C @n)
                                 ps = collect t $ b^.controlPoints
                             in ( fromPointSeq . Seq.take (n + 1) $ ps
                                , fromPointSeq . Seq.drop n       $ ps
                                )

collect   :: (Arity d, Ord r, Num r) => r -> LSeq n (Point d r) -> Seq (Point d r)
collect t = go . LSeq.toSeq
  where
    go = \case
      ps@(_ :<| Empty) -> ps
      ps@(p :<| tl)    -> let (ini :|> q) = ps in (p :<| go (Seq.zipWith blend ini tl)) :|> q
      _                -> error "collect: absurd"

    blend p q = p .+^ t *^ (q .-. p)

-- {-

-- -- | Merge to Bezier pieces. Assumes they can be merged into a single piece of the same degree
-- --   (as would e.g. be the case for the result of a 'split' operation).
-- --   Does not test whether this is the case!
-- merge :: (Arity d, Ord r, Num r) => (Bezier d r, Bezier d r) -> Bezier d r

-- -}

-- | Approximate Bezier curve by Polyline with given resolution.
approximate :: forall n d r. (KnownNat n, Arity d, Ord r, Fractional r)
            => r -> BezierSpline n d r -> [Point d r]
approximate eps b
    | squaredEuclideanDist p q < eps^2 = [p,q]
    | otherwise                        = let (b1, b2) = split 0.5 b
                                         in approximate eps b1 ++ tail (approximate eps b2)
  where
    p = b^.controlPoints.to LSeq.head
    q = b^.controlPoints.to LSeq.last

-- | Given a point on (or close to) a Bezier curve, return the corresponding parameter value.
--   (For points far away from the curve, the function will return the parameter value of
--   an approximate locally closest point to the input point.)
parameterOf      :: (Arity d, Ord r, Fractional r) => BezierSpline n d r -> Point d r -> r
parameterOf b p = binarySearch (qdA p . evaluate b) treshold (1 - treshold)
  where treshold = 0.0001

binarySearch                                    :: (Ord r, Fractional r) => (r -> r) -> r -> r -> r
binarySearch f l r | abs (f l - f r) < treshold = m
                   | derivative f m  > 0        = binarySearch f l m
                   | otherwise                  = binarySearch f m r
  where m = (l + r) / 2
        treshold = 0.0001

derivative     :: Fractional r => (r -> r) -> r -> r
derivative f x = (f (x + delta) - f x) / delta
  where delta = 0.00001

-- | Snap a point close to a Bezier curve to the curve.
snap   :: (Arity d, Ord r, Fractional r) => BezierSpline n d r -> Point d r -> Point d r
snap b = evaluate b . parameterOf b
