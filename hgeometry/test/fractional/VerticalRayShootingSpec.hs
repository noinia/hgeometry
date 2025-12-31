module VerticalRayShootingSpec where

import Data.Ord
import Data.Maybe
import Control.Lens
import Data.Foldable
import HGeometry.VerticalRayShooting.PersistentSweep
import HGeometry.Ext
import HGeometry.Vector
import HGeometry.Intersection
import HGeometry.HalfLine
import Data.List.NonEmpty qualified as NonEmpty
import Data.List.NonEmpty (NonEmpty(..))
import HGeometry.LineSegment
import HGeometry.Point
import Test.Hspec
import R
--------------------------------------------------------------------------------

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
         it "test2Segments tests" $ do
           let ds = fromJust $ verticalRayShootingStructure' test2Segments
               -- we have at least one non-vertical segment so the fromJust is safe
           for_ test2Queries $ \(q :+ answer) ->
             (view extra <$> segmentAbove q ds) `shouldBe` Just answer
         it "test2Segments tests ; same as naive " $ do
           let segs = (^.core) <$> test2Segments
               ds   = fromJust $ verticalRayShootingStructure' segs
               -- we have at least one non-vertical segment, so the fromJust is safe
           for_ test2Queries $ \q ->
             segmentAbove q ds `shouldBe` naiveSegmentAbove q segs


test1 :: VerticalRayShootingStructure (ClosedLineSegment (Point 2 R) :+ Int)
test1 = verticalRayShootingStructure . NonEmpty.fromList $ zipWith (:+)
        [ hor 2 0 10
        , hor 4 1 12
        , hor 2 10 14
        ] [1..]
  where
    hor y l r = ClosedLineSegment (Point2 l y) (Point2 r y)



test2Queries :: [Point 2 R :+ Int]
test2Queries = [ Point2 450 0  :+  11 -- the int is the segment number we should be hitting
               , Point2 100 40 :+  14
               ]

test2Segments :: NonEmpty (ClosedLineSegment (Point 2 R) :+ Int)
test2Segments = NonEmpty.fromList
                [ ClosedLineSegment (Point2 (-10) (-10)) (Point2 40 (-10))      :+ 1
                , ClosedLineSegment (Point2 (-10) (-10)) (Point2 (-10) 15)      :+ 2
                , ClosedLineSegment (Point2 (-10) 15)    (Point2 (-10) 190)     :+ 3
                , ClosedLineSegment (Point2 (-10) 190)   (Point2 (-10) 1010)    :+ 4
                , ClosedLineSegment (Point2 40 (-10))    (Point2 90 (-10))      :+ 5
                , ClosedLineSegment (Point2 (-10) 15)    (Point2 40 (-10))      :+ 6
                , ClosedLineSegment (Point2 90 (-10))    (Point2 140 (-10))     :+ 7
                , ClosedLineSegment (Point2 (-10) 190)   (Point2 90 (-10))      :+ 9
                , ClosedLineSegment (Point2 140 (-10))   (Point2 1010 (-10))    :+ 10
                , ClosedLineSegment (Point2 140 (-10))   (Point2 1010 860)      :+ 11
                , ClosedLineSegment (Point2 1010 (-10))  (Point2 1010 860)      :+ 12
                , ClosedLineSegment (Point2 1010 860)    (Point2 1010 1010)     :+ 13
                , ClosedLineSegment (Point2 (-10) 1010)  (Point2 1010 1010)     :+ 14
                ]


--------------------------------------------------------------------------------

-- | intersections with vertical segments are ignored
naiveSegmentAbove   :: ( LineSegment_ lineSegment point, Point_ point 2 r
                       , Point_ queryPoint 2 r
                       , Ord r, Num r
                       , Foldable set

                       , IsIntersectableWith (HalfLine (Point 2 r)) lineSegment
                       , Intersection (HalfLine (Point 2 r)) lineSegment
                        ~ Maybe (HalfLineLineSegmentIntersection (Point 2 r) lineSegment)
                       ) => queryPoint -> set lineSegment -> Maybe lineSegment
naiveSegmentAbove q = lowest . mapMaybe intersectWith . toList
  where
    ray = HalfLine (q^.asPoint) (Vector2 0 1) -- upward ray
    intersectWith seg = case ray `intersect` seg of
      Just (HalfLine_x_LineSegment_Point p) -> Just (p :+ seg)
      _                                     -> Nothing

    lowest = fmap (^.extra) . minimumByOf folded (comparing (^.core.yCoord))
