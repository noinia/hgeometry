module Geometry.BallSpec where

import Control.Lens
import Control.Monad (forM_)
import Data.Ext
import Geometry.Ball
import Geometry.LineSegment
import Geometry.Point
import Data.Intersection
import Data.RealNumber.Rational
import Test.Hspec
import Test.QuickCheck
import Test.Util

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
        let mySeg = ClosedLineSegment (ext $ Point2 @Point @R (-1) 1) (ext $ Point2 1 1)
        (mySeg `intersects` unitCircle @R) `shouldBe` True


unitCircle :: Num r => Circle () r
unitCircle = Circle (ext origin) 1

segment     :: Floating r => r -> r -> LineSegment 2 () r
segment r x = ClosedLineSegment (ext origin) (ext $ Point2 (r*cos x) (r*sin x))
