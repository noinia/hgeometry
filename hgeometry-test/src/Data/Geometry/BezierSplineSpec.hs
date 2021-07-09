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
           it "approximate" $ property $ \(T r) (b :: BezierSpline 3 2 R) ->
             -- approximate r b == Maarten.approximate r (toMaartenBezier b)
             testApproximate r b

             -- note that this currently tests only for some limited range of r (between 0 and 1)
           it "parameterOf" $ property $ \(T t) (b :: BezierSpline 3 2 R) ->
             let p = evaluate b t in
             parameterOf b p == Maarten.parameterOf (toMaartenBezier b) p
             -- note that this currently tests only points *on* the curve

testApproximate :: R -> BezierSpline 3 2 R -> Bool
testApproximate treshold curve =
  let polyline  = approximate treshold curve
      segs      = edgeSegments polyline
      midpoints = map (\seg -> average [view (start . core) seg, view (end . core) seg]) $ F.toList segs
  in all (testSnap treshold curve) midpoints

testSnap :: R -> BezierSpline 3 2 R -> Point 2 R -> Bool
testSnap treshold curve point = squaredEuclideanDist point (snap curve point) <= treshold ^ 2

average :: (Functor t, Foldable t, Arity d, Fractional r) => t (Point d r) -> Point d r
average ps = origin .+^ foldr1 (^+^) (fmap toVec ps) ^/ realToFrac (length ps)
