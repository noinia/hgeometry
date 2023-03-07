module HGeometry.HalfSpaceSpec
  (spec) where

import HGeometry.HalfSpace
import HGeometry.HyperPlane
import HGeometry.Line
import HGeometry.Point
import HGeometry.Intersection
import Test.Hspec

type R = Double

myHalfspace :: HalfSpaceF (LineEQ R)
myHalfspace = HalfSpace (LineEQ 1 2)

myPoints :: [(Point 2 R, Bool)]
myPoints = [ (Point2 10 10, False)
           , (Point2 10 1000, True)
           , (Point2 0 20, True)
           ]

spec :: Spec
spec = pure ()
  -- describe "halfspace Tests" $ do
  --        it "in halfspace" $ do
  --          mapM_ (\(q,ans) -> (q `intersects` myHalfspace) `shouldBe` ans) myPoints
