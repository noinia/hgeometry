{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module LineSegment.Intersection.BentleyOttmannSpec
  (spec
  ) where

import           HGeometry.LineSegment.Intersection.BentleyOttmann
-- import           Algorithms.Geometry.LineSegmentIntersection
import qualified HGeometry.LineSegment.Intersection.BentleyOttmann as Sweep
import qualified HGeometry.LineSegment.Intersection.Naive as Naive
import           Control.Lens
import           HGeometry.Ext
import           HGeometry.LineSegment
import           HGeometry.Point
import           LineSegmentSpec (arrowAsOpen)
import qualified Data.Map as Map
import           HGeometry.Number.Real.Rational
import           Ipe
import           System.OsPath
import           Golden
import           Test.Hspec

--------------------------------------------------------------------------------

type R = RealNumber 5


spec :: Spec
spec = do
  describe "Testing Bentley Ottmann LineSegment Intersection" $ do
    -- toSpec (TestCase "myPoints" myPoints)
    -- toSpec (TestCase "myPoints'" myPoints')
    ipeSpec

testPath = [osp|test-with-ipe//LineSegment/Intersection/|]

ipeSpec :: Spec
ipeSpec = do -- testCases (testPath <> [osp|open_bug.ipe|])
             testCases (testPath <> [osp|manual.ipe|])
             -- testCases (testPath <> [osp|open.ipe|])


-- openCorrect (TestCase name segs) =



testCases    :: OsPath -> Spec
testCases fp = (runIO $ readInput =<< getDataFileName fp) >>= \case
    Left e    -> it "reading LineSegment Intersection file" $
                   expectationFailure $ "Failed to read ipe file " ++ show e
    Right tcs -> mapM_ toSpec tcs


-- | Point sets per color, Crosses form the solution
readInput    :: OsPath -> IO (Either ConversionError [TestCase R])
readInput fp = fmap f <$> readSinglePageFile fp
  where
    f page = [TestCase (takeFileName fp) segs]
      where
        segs = map (view core . arrowAsOpen) . readAll $ page




data TestCase r = TestCase { _name :: OsString, _segments :: [LineSegment AnEndPoint (Point 2 r)]
                           } deriving (Show,Eq)

toSpec                      :: (Fractional r, Ord r, Show r) => TestCase r -> Spec
toSpec (TestCase name segs) = describe ("testing segments from " <> show name) $ do
                                samePointsAsNaive segs
                                sameAsNaive segs
                                -- it "bug" $ do
                                --   Map.toList (Naive.intersections segs) `shouldBe` []

-- | Test if we have the same intersection points
samePointsAsNaive      :: ( LineSegment_ lineSegment point
                          , Point_ point 2 r
                          , Eq lineSegment
                          , Ord r, Fractional r
                          , HasOnSegment lineSegment 2
                          , IntersectConstraints lineSegment
                          , StartPointOf lineSegment ~ EndPointOf lineSegment
                          , Show lineSegment, Show r
                          )
                       => [lineSegment] -> Spec
samePointsAsNaive segs = it "Same points as Naive" $ do
  Map.keys (Sweep.intersections segs) `shouldBe` Map.keys (Naive.intersections segs)

-- | Test if they every intersection point has the right segments
sameAsNaive      :: ( LineSegment_ lineSegment point
                    , Point_ point 2 r
                    , Eq lineSegment
                    , Ord r, Fractional r
                    , HasOnSegment lineSegment 2
                    , IntersectConstraints lineSegment
                    , StartPointOf lineSegment ~ EndPointOf lineSegment
                    , Show lineSegment, Show r
                    )
                 => [lineSegment] -> Spec
sameAsNaive segs = it "Same as Naive " $ do
                     Sweep.intersections segs `shouldBe` Naive.intersections segs
