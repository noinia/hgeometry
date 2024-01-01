module HGeometry.BallSpec where

-- import Control.Lens
import Control.Monad (forM_)
-- import HGeometry.Ext
import HGeometry.Number.Real.Rational
import HGeometry.Ball
import HGeometry.Intersection
import HGeometry.LineSegment
import HGeometry.Point
-- import HGeometry.Vector
import Test.Hspec
-- import Test.QuickCheck
-- import Test.Util

--------------------------------------------------------------------------------

type R = RealNumber 5


spec :: Spec
spec = do
  describe "Ball" $ do
    describe "intersection tests" $ do
      it "grav2ity's tests with Double" $
        forM_ [1.0,1.1..1.6] $ \r ->
          (segment r (0.1 :: Double) `intersects` unitCircle @Double) `shouldBe` True
      it "touching line segment" $ do
        let mySeg = ClosedLineSegment (Point2 @R (-1) 1) (Point2 1 1)
        (mySeg `intersects` unitCircle @R) `shouldBe` True


unitCircle :: (Num r) => Circle (Point 2 r)
unitCircle = Circle origin 1

segment     :: (Floating r)
            => r -> r -> ClosedLineSegment (Point 2 r)
segment r x = ClosedLineSegment origin (Point2 (r*cos x) (r*sin x))
