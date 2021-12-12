{-# LANGUAGE OverloadedStrings          #-}
module Algorithms.Geometry.LineSegmentIntersection.BentleyOttmannSpec (spec) where

import           Algorithms.Geometry.LineSegmentIntersection
import qualified Algorithms.Geometry.LineSegmentIntersection.BentleyOttmann as Sweep
import qualified Algorithms.Geometry.LineSegmentIntersection.Naive as Naive
import           Control.Lens
import           Data.Ext
import           Data.Geometry.LineSegment
import           Data.Geometry.Polygon
import           Data.List (intercalate)
import qualified Data.Map as Map
import           Data.Proxy
import           Data.RealNumber.Rational
import           Ipe
import           Paths_hgeometry_test
import           Test.Hspec

type R = RealNumber 5


spec :: Spec
spec = do
  describe "Testing Bentley Ottmann LineSegment Intersection" $ do
    -- toSpec (TestCase "myPoints" myPoints)
    -- toSpec (TestCase "myPoints'" myPoints')
    ipeSpec

testPath = "src/Algorithms/Geometry/LineSegmentIntersection/"

ipeSpec :: Spec
ipeSpec = do testCases (testPath <> "manual.ipe")
             -- testCases (testPath <> "open.ipe")

testCases    :: FilePath -> Spec
testCases fp = (runIO $ readInput =<< getDataFileName fp) >>= \case
    Left e    -> it "reading LineSegment Intersection file" $
                   expectationFailure $ "Failed to read ipe file " ++ show e
    Right tcs -> mapM_ toSpec tcs


-- | Point sets per color, Crosses form the solution
readInput    :: FilePath -> IO (Either ConversionError [TestCase R])
readInput fp = fmap f <$> readSinglePageFile fp
  where
    f page = [TestCase segs]
      where
        segs = page^..content.traverse._IpePath.core._asLineSegment



data TestCase r = TestCase { _segments :: [LineSegment 2 () r]
                           } deriving (Show,Eq)


toSpec                 :: (Fractional r, Ord r, Show r) => TestCase r -> Spec
toSpec (TestCase segs) = describe ("testing segments ") $ do
                            samePointsAsNaive segs
                            sameAsNaive $ take 5 segs

-- | Test if we have the same intersection points
samePointsAsNaive segs = it "Same points as Naive" $ do
  (Map.keys $ Sweep.intersections segs) `shouldBe` (Map.keys $ Naive.intersections segs)

-- | Test if they every intersection point has the right segments
sameAsNaive      :: (Fractional r, Ord r, Eq p
                    , Show p, Show r
                    ) => [LineSegment 2 p r] -> Spec
sameAsNaive segs = it "Same as Naive " $ do
    (Sweep.intersections segs) `shouldBe` (Naive.intersections segs)


-- -- | For each intersection point the segments intersecting there.
-- newtype Intersections' p r = Intersections { asMap :: Intersections p r }

-- toIntersectionPoints :: Intersections' p r -> [IntersectionPoint p r]
-- toIntersectionPoints = map (uncurry IntersectionPoint) . Map.toAscList . asMap

-- instance (Show p, Show r) => Show (Intersections' p r) where
--   show = ("\n" <> ) . intercalate "\n" . map show . toIntersectionPoints

-- instance (Ord r, Num r, Eq p) => Eq (Intersections' p r) where
--   is == is' = toIntersectionPoints is == toIntersectionPoints is'



      --  expected:


      -- fromList [(Point2 144 80,Associated {_startPointOf = fromList [AroundEnd (ClosedLineSegment (Point2 144 80 :+ ()) (Point2 225.708 52.1085 :+ ()))], _endPointOf = fromList [AroundStart (ClosedLineSegment (Point2 16 16 :+ ()) (Point2 144 80 :+ ()))], _interiorTo = fromList []})
      --          ,(Point2 144.00052~ 79.99982~,Associated {_startPointOf = fromList [], _endPointOf = fromList [], _interiorTo = fromList [AroundIntersection (ClosedLineSegment (Point2 144 80 :+ ()) (Point2 225.708 52.1085 :+ ())),AroundIntersection (ClosedLineSegment (Point2 137.6866 70.548 :+ ()) (Point2 148.53751~ 86.79160~ :+ ()))]}),(Point2 183.32299~ 66.57686~,Associated {_startPointOf = fromList [], _endPointOf = fromList [], _interiorTo = fromList [AroundIntersection (ClosedLineSegment (Point2 144 80 :+ ()) (Point2 225.708 52.1085 :+ ())),AroundIntersection (ClosedLineSegment (Point2 128 32 :+ ()) (Point2 256 112 :+ ()))]})]



      --   but got: fromList [(Point2 144 80,Associated {_startPointOf = fromList [AroundEnd (ClosedLineSegment (Point2 144 80 :+ ()) (Point2 225.708 52.1085 :+ ()))], _endPointOf = fromList [AroundStart (ClosedLineSegment (Point2 16 16 :+ ()) (Point2 144 80 :+ ()))], _interiorTo = fromList [AroundIntersection (ClosedLineSegment (Point2 144 80 :+ ()) (Point2 225.708 52.1085 :+ ())),AroundIntersection (ClosedLineSegment (Point2 16 16 :+ ()) (Point2 144 80 :+ ()))]}),(Point2 144.00052~ 79.99982~,Associated {_startPointOf = fromList [], _endPointOf = fromList [], _interiorTo = fromList [AroundIntersection (ClosedLineSegment (Point2 144 80 :+ ()) (Point2 225.708 52.1085 :+ ())),AroundIntersection (ClosedLineSegment (Point2 137.6866 70.548 :+ ()) (Point2 148.53751~ 86.79160~ :+ ()))]}),(Point2 183.32299~ 66.57686~,Associated {_startPointOf = fromList [], _endPointOf = fromList [], _interiorTo = fromList [AroundIntersection (ClosedLineSegment (Point2 144 80 :+ ()) (Point2 225.708 52.1085 :+ ())),AroundIntersection (ClosedLineSegment (Point2 128 32 :+ ()) (Point2 256 112 :+ ()))]})]
