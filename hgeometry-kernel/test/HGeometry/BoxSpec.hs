module HGeometry.BoxSpec where

import Control.Lens
import Data.Maybe
import Data.Ratio
import HGeometry.Box
import HGeometry.Intersection
import HGeometry.Interval
import HGeometry.Kernel.Instances ()
import HGeometry.Line.LineEQ
import HGeometry.LineSegment
import HGeometry.Number.Real.Rational (RealNumber)
import HGeometry.Point
import HGeometry.Vector
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
-- import Test.Util

--------------------------------------------------------------------------------

type R = RealNumber 5

-- arbitraryPointInBoundingBox   :: Box (Point 2 Rational) -> Gen (Point 2 Rational)
-- arbitraryPointInBoundingBox b = do
--   ZeroToOne rX <- arbitrary
--   ZeroToOne rY <- arbitrary
--   let minPt        = b^.minPoint
--       offsetVector = Vector2 (width b * rX) (height b * rY)
--   pure $ minPt .+^ offsetVector

myRect :: Rectangle (Point 2 Double)
myRect = Rectangle (Point2 1 1) (Point2 10 20.0)

spec :: Spec
spec = do
  describe "Box" $ do
    it "vector closed interval test" $ do
      let intervalVec :: Vector 2 (ClosedInterval Int)
          intervalVec = Vector2 (ClosedInterval 5 10) (ClosedInterval 20 40)
      (intervalVec&components %~ view start) `shouldBe` Vector2 5 20
    it "show Box" $ do
      show myRect `shouldBe` "Box (Point2 1.0 1.0) (Point2 10.0 20.0)"
    it "size" $
      size myRect `shouldBe` Vector2 9 19
    describe "intersection tests with lines " $ do
      let myIncLine = LineEQ 2 (-3)  :: LineEQ Double
          myDecLine = LineEQ (-1) 30 :: LineEQ Double
      it "intersects" $ do
        (myIncLine `intersects` myRect) `shouldBe` True
        (myDecLine `intersects` myRect) `shouldBe` True
      it "intersect" $ do
        (myIncLine `intersect` myRect)
          `shouldBe`
          Just (Line_x_Box_Segment (ClosedLineSegment (Point2 2 1) (Point2 10.0 17.0)))
        (myDecLine `intersect` myRect)
          `shouldBe`
          Just (Line_x_Box_Point (Point2 10 20))
      prop "intersect agrees with intersects" $
        \(l :: LineEQ R) (r :: Rectangle (Point 2 R)) ->
          l `intersects` r == isJust (l `intersect` r)

--     it "intersect tests" $
--       ((boundingBoxList' $ [Point2 (-4) (-3), Point2 (-4) (10 :: Int)])
--        `intersects`
--        (boundingBoxList' $ [Point2 (-5) 1, Point2 (-4) (0 :: Int)]))
--       `shouldBe` True


newtype ZeroToOne = ZeroToOne Rational

instance Show ZeroToOne where
  show (ZeroToOne r) = show r

instance Arbitrary ZeroToOne where
  arbitrary = do
    k <- chooseInteger (0, granularity)
    pure $ ZeroToOne $ k % granularity
    where
      granularity = 1000000
  shrink (ZeroToOne 1) = []
  shrink (ZeroToOne 0) = []
  shrink (ZeroToOne r) = [ ZeroToOne $ div (numerator r) 2 % div (denominator r) 2]
