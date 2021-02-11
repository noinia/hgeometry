module Data.Geometry.VerticalRayShooting.VerticalRayShootingSpec where

import           Data.Geometry.VerticalRayShooting.PersistentSweep
import           Control.Lens hiding (contains, below)
import           Data.Ext
import           Data.Geometry.LineSegment
import           Data.Geometry.Point
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.RealNumber.Rational
import           Test.Hspec

--------------------------------------------------------------------------------

type R = RealNumber 5

spec :: Spec
spec = describe "VerticalRayShooting Tests" $ do
         it "manual queries on horizontal subidv" $ do
           segmentAbove (Point2 5 0) test1 `shouldBe` Just (LineSegment (Closed (Point2 0 2 :+ ())) (Closed (Point2 10 2 :+ ())) :+ 1)
           segmentAbove (Point2 5 2) test1 `shouldBe` Just (LineSegment (Closed (Point2 1 4 :+ ())) (Closed (Point2 12 4 :+ ())) :+ 2)
           segmentAbove (Point2 5 1) test1 `shouldBe` Just (LineSegment (Closed (Point2 0 2 :+ ())) (Closed (Point2 10 2 :+ ())) :+ 1)
           segmentAbove (Point2 5 5) test1 `shouldBe` Nothing
           segmentAbove (Point2 10 5) test1 `shouldBe` Nothing
           segmentAbove (Point2 10 0) test1 `shouldBe` Just (LineSegment (Closed (Point2 0 2 :+ ())) (Closed (Point2 10 2 :+ ())) :+ 1)
           segmentAbove (Point2 10 2) test1 `shouldBe` Just (LineSegment (Closed (Point2 1 4 :+ ())) (Closed (Point2 12 4 :+ ())) :+ 2)
           segmentAbove (Point2 10 1) test1 `shouldBe` Just (LineSegment (Closed (Point2 0 2 :+ ())) (Closed (Point2 10 2 :+ ())) :+ 1)

test1 :: VerticalRayShootingStructure () Int R
test1 = verticalRayShootingStructure . NonEmpty.fromList $ zipWith (:+)
        [ hor 2 0 10
        , hor 4 1 12
        , hor 2 10 14
        ] [1..]
  where
    hor y l r = ClosedLineSegment (ext $ Point2 l y) (ext $ Point2 r y)
