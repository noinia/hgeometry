{-# LANGUAGE TypeApplications #-}
module Data.Geometry.BezierSplineSpec where

import           Control.Lens
import           Data.Ext
import qualified Data.Foldable as F
import qualified Data.Geometry.BezierMaarten as Maarten
import           Data.Geometry.BezierSpline
import           Data.Geometry.PolyLine
import           Data.Geometry.LineSegment
import           Data.Geometry.Point
import           Data.Geometry.Vector hiding (init)
import           Data.Range
import           Data.RealNumber.Rational
import           Test.Hspec
import           Test.QuickCheck
--------------------------------------------------------------------------------

type R = RealNumber 5

toMaartenBezier :: BezierSpline n d r -> Maarten.Bezier d r
toMaartenBezier = Maarten.Bezier . ctrlPts

ctrlPts   :: BezierSpline n d r -> [Point d r]
ctrlPts b = F.toList $ b^.controlPoints

newtype T = T R deriving (Show,Eq,Ord,Num,Real,Fractional)

instance Arbitrary T where
  arbitrary = realToFrac <$> choose (0,1 :: Double)


spec :: Spec
spec = specND -- (C @3) (C @2)

specND :: Spec
specND = describe "BezierSpline" $ do
           it "evaluate" $ property $ \(T t) (b :: BezierSpline 3 2 R) ->
             evaluate b t == Maarten.evaluate (toMaartenBezier b) t
           it "tangent" $ property $ \(b :: BezierSpline 3 2 R) ->
             tangent b == Maarten.tangent (toMaartenBezier b)
           it "subBezier" $ property $ \(Range' (T t) (T u)) (b :: BezierSpline 3 2 R) ->
             (toMaartenBezier $ subBezier t u b) == Maarten.subBezier t u (toMaartenBezier b)
           it "split" $ property $ \(T t) (b :: BezierSpline 3 2 R) ->
             bimap toMaartenBezier toMaartenBezier (split t b)
               == Maarten.split t (toMaartenBezier b)
           it "approximate" $ property $ \(T treshold) (b :: BezierSpline 3 2 R) ->
             -- approximate r b == Maarten.approximate r (toMaartenBezier b)
             testApproximate treshold b

             -- note that this currently tests only for some limited range of r (between 0 and 1)
           it "parameterOf" $ property $ \(T treshold) (T t) (randomVector :: Vector 2 R) (b :: BezierSpline 3 2 R) ->
             let perturbation = makePerturbation treshold randomVector
                 p  = evaluate b t .+^ perturbation
                 t' = parameterOf treshold b p
                 p' = evaluate b t'
             in squaredEuclideanDist p p' < 2 * treshold^2

-- | This function tests whether the result of approximate is indeed within treshold from the curve.
--   The implementation is not exact: it takes the midpoint of each segment of the approximation and
--   snaps it to the curve. 
testApproximate :: R -> BezierSpline 3 2 R -> Bool
testApproximate treshold curve =
  let Just polyline  = fromPoints $ map (:+ ()) $ approximate treshold curve
      segs      = edgeSegments polyline
      midpoints = map (\seg -> average [view (start . core) seg, view (end . core) seg]) $ F.toList segs
  in all (testSnap treshold curve) midpoints

testSnap :: R -> BezierSpline 3 2 R -> Point 2 R -> Bool
testSnap treshold curve point = squaredEuclideanDist point (snap treshold curve point) <= 2 * treshold ^ 2

average :: (Functor t, Foldable t, Arity d, Fractional r) => t (Point d r) -> Point d r
average ps = origin .+^ foldr1 (^+^) (fmap toVec ps) ^/ realToFrac (length ps)

-- | Create a perturbation vector of length r from a random vector.
--   I don't know what range the random vector is in, and we can't compute the norm
--   since it requires Floating R - so this is somewhat of a hack.
makePerturbation :: R -> Vector 2 R -> Vector 2 R
makePerturbation r v = 
  let p = origin .+^ v :: Point 2 R
      m = max (abs $ view xCoord p) (abs $ view yCoord p)
      factor | m == 0    = 1
             | otherwise = r / m
  in factor *^ v