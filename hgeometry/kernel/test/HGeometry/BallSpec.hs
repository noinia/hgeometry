module HGeometry.BallSpec where

import Control.Lens
import Control.Monad (forM_)
-- import HGeometry.Ext
import HGeometry.Number.Real.Rational
import HGeometry.Ball
import HGeometry.Intersection
import HGeometry.LineSegment
import HGeometry.Point
import HGeometry.Kernel.Instances ()
import HGeometry.Boundary
import HGeometry.HalfLine
import HGeometry.Vector
import HGeometry.HyperPlane
import HGeometry.HalfSpace
import HGeometry.Line.PointAndVector
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck ((===),(==>), Discard(..), property)
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


      it "closest point to ray" $
        let ray      = HalfLine (Point3 (0 :: R) (1/2) 10000) (Vector3 0 0 (-1))
        in pointClosestTo (origin :: Point 3 R) ray `shouldBe` (Point3 0 (1/2) 0)

      it "closest point to line" $
        let ray      = LinePV (Point3 (0 :: R) (1/2) 10000) (Vector3 0 0 (-1))
        in pointClosestTo (origin :: Point 3 R) ray `shouldBe` (Point3 0 (1/2) 0)

      it "proper halfspace" $
        let p = Point3 0 (1/2) 10000  :: Point 3 R
            v = Vector3 0 0 (-1)
            h = HalfSpace Positive (fromPointAndNormal p v) :: HalfSpace 3 R
        in (h^.boundingHyperPlane.to normalVector) `shouldBe` v


      it "ball intersects ray" $
        let ray      = HalfLine (Point3 (0 :: R) (1/2) 10000) (Vector3 0 0 (-1))
        in (ray `intersects` (unitBall :: Ball (Point 3 R))) `shouldBe`  True

    boundaryPointsSpec
    diametralSpec

unitCircle :: (Num r) => Circle (Point 2 r)
unitCircle = Circle origin 1

segment     :: (Floating r)
            => r -> r -> ClosedLineSegment (Point 2 r)
segment r x = ClosedLineSegment origin (Point2 (r*cos x) (r*sin x))


boundaryPointsSpec :: Spec
boundaryPointsSpec = describe "BoundaryPoints" $ do
    prop "center in disk" $
      \a b (c :: Point 2 R) ->
        case diskFromPoints a b c of
          Just disk -> (disk^.center) `inBall` disk === Inside
          Nothing   -> property Discard

    prop "on boundary correct" $
      \a b (c :: Point 2 R) ->
        case diskFromPoints a b c of
          Just disk -> property $ all (\q -> q `inBall` disk == OnBoundary) [a,b,c]
          Nothing   -> property Discard

diametralSpec :: Spec
diametralSpec = describe "Diametral" $ do
    prop "center in disk" $
      \a (b :: Point 2 R) ->
        a /= b ==> let disk = DiametralPoints a b
                   in (disk^.center) `inBall` disk === Inside

    prop "on boundary correct" $
      \a (b :: Point 2 R) ->
        a /= b ==> let disk = DiametralPoints a b
                   in a `inBall` disk == OnBoundary && b `inBall` disk == OnBoundary
