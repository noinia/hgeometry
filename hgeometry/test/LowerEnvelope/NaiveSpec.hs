module LowerEnvelope.NaiveSpec(spec) where

import HGeometry.Combinatorial.Util
import HGeometry.HyperPlane.Class
import HGeometry.HyperPlane.NonVertical
import HGeometry.Instances ()
import HGeometry.LowerEnvelope
import HGeometry.Number.Real.Rational
import HGeometry.Point
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Instances ()

--------------------------------------------------------------------------------

type R = RealNumber 5

spec :: Spec
spec = describe "lowerEnvelope tests" $ do
         prop "intersection on plane" $ \h1 h2 (h3 :: Plane R) ->
           case verifyOnPlane h1 h2 h3 of
             Nothing     -> True
             Just (_,bs) -> and bs


-- | verify that the intersection point indeed lis on all three planes
verifyOnPlane          :: (Fractional r, Eq r)
                       => Plane r -> Plane r -> Plane r -> Maybe (Point 3 r, Three Bool)
verifyOnPlane h1 h2 h3 = f <$> intersectionPoint h1 h2 h3
  where
    f p = (p, g p <$> Three h1 h2 h3)
    g (Point3 x y z) h = evalAt (Point2 x y) h == z


myEnv = lowerEnvelope inputs
myTriEnv = triangulatedLowerEnvelope inputs

inputs = []
