module LowerEnvelope.NaiveSpec where

import qualified Data.Vector as Boxed
import           HGeometry.Combinatorial.Util
import           HGeometry.HyperPlane.Class
import           HGeometry.HyperPlane.NonVertical
import           HGeometry.Instances ()
import           HGeometry.LowerEnvelope
import           HGeometry.Number.Real.Rational
import           HGeometry.Point
import           HGeometry.Vector
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

--------------------------------------------------------------------------------

type R = RealNumber 5

spec :: Spec
spec = describe "lowerEnvelope tests" $ do
         prop "intersection on plane" $ \h1 h2 (h3 :: Plane R) ->
           case verifyOnPlane h1 h2 h3 of
             Nothing     -> True
             Just (_,bs) -> and bs
         it "asVertex" $
           let [h1,h2,h3] = inputs in
           asVertex (Three h1 h2 h3) `shouldBe` Just (Vertex (Point3 10 10 10) (Vector3 h1 h2 h3))
         prop "belowall" $ \h1 h2 h3 ->
           case asVertex (Three h1 h2 (h3 :: Plane R)) of
             Nothing -> True
             Just v  -> v `belowAll` [h1,h2,h3]
         it "vertices inputs" $
           let [h1,h2,h3] = inputs
               v          = Vertex (Point3 10 10 10) (Vector3 h1 h2 h3)
           in
           lowerEnvelope inputs
           `shouldBe`
           LowerEnvelope [v] (Boxed.fromList [])


-- | verify that the intersection point indeed lis on all three planes
verifyOnPlane          :: (Fractional r, Eq r)
                       => Plane r -> Plane r -> Plane r -> Maybe (Point 3 r, Three Bool)
verifyOnPlane h1 h2 h3 = f <$> intersectionPoint h1 h2 h3
  where
    f p = (p, g p <$> Three h1 h2 h3)
    g (Point3 x y z) h = evalAt (Point2 x y) h == z


myEnv = lowerEnvelope inputs
myTriEnv = triangulatedLowerEnvelope inputs

inputs :: [Plane R]
inputs = [ Plane 1 0 0
         , Plane 0 1 0
         , Plane 0 0 10
         ]
