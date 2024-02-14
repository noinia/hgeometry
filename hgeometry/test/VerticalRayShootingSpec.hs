module VerticalRayShootingSpec where

import           HGeometry.VerticalRayShooting.PersistentSweep
import           HGeometry.Ext
import qualified Data.List.NonEmpty as NonEmpty
import           HGeometry.Number.Real.Rational
import           HGeometry.LineSegment
import           HGeometry.Point
import           Test.Hspec

--------------------------------------------------------------------------------

type R = RealNumber 5

spec :: Spec
spec = describe "VerticalRayShooting Tests" $ do
         it "manual queries on horizontal subidv" $ do
           segmentAbove (Point2 5 0) test1 `shouldBe`
             Just (ClosedLineSegment (Point2 0 2) (Point2 10 2) :+ 1)
           segmentAbove (Point2 5 2) test1 `shouldBe`
             Just (ClosedLineSegment (Point2 1 4) (Point2 12 4) :+ 2)
           segmentAbove (Point2 5 1) test1 `shouldBe`
             Just (ClosedLineSegment (Point2 0 2) (Point2 10 2) :+ 1)
           segmentAbove (Point2 5 5) test1 `shouldBe` Nothing
           segmentAbove (Point2 10 5) test1 `shouldBe` Nothing
           segmentAbove (Point2 10 0) test1 `shouldBe`
             Just (ClosedLineSegment (Point2 0 2) (Point2 10 2) :+ 1)
           segmentAbove (Point2 10 2) test1 `shouldBe`
             Just (ClosedLineSegment (Point2 1 4) (Point2 12 4) :+ 2)
           segmentAbove (Point2 10 1) test1 `shouldBe`
             Just (ClosedLineSegment (Point2 0 2) (Point2 10 2) :+ 1)

test1 :: VerticalRayShootingStructure (ClosedLineSegment (Point 2 R) :+ Int)
test1 = verticalRayShootingStructure . NonEmpty.fromList $ zipWith (:+)
        [ hor 2 0 10
        , hor 4 1 12
        , hor 2 10 14
        ] [1..]
  where
    hor y l r = ClosedLineSegment (Point2 l y) (Point2 r y)
