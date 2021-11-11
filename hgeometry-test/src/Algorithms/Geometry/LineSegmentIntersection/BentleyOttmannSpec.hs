{-# LANGUAGE OverloadedStrings          #-}
module Algorithms.Geometry.LineSegmentIntersection.BentleyOttmannSpec (spec) where

import           Algorithms.Geometry.LineSegmentIntersection
import qualified Algorithms.Geometry.LineSegmentIntersection.BentleyOttmann as Sweep
import qualified Algorithms.Geometry.LineSegmentIntersection.Naive as Naive
import           Control.Lens
import           Data.Ext
import           Ipe
import           Data.Geometry.LineSegment
import           Data.Geometry.Polygon
import qualified Data.Map as Map
import           Data.Proxy
import           Paths_hgeometry_test
import           Test.Hspec

spec :: Spec
spec = do
  describe "Testing Bentley Ottmann LineSegment Intersection" $ do
    -- toSpec (TestCase "myPoints" myPoints)
    -- toSpec (TestCase "myPoints'" myPoints')
    ipeSpec
  describe "Self Intersecting Polygon Tests" $ do
    siTestCases (testPath <> "selfIntersections.ipe")

testPath = "src/Algorithms/Geometry/LineSegmentIntersection/"

ipeSpec :: Spec
ipeSpec = testCases (testPath <> "manual.ipe")

testCases    :: FilePath -> Spec
testCases fp = (runIO $ readInput =<< getDataFileName fp) >>= \case
    Left e    -> it "reading LineSegment Intersection file" $
                   expectationFailure $ "Failed to read ipe file " ++ show e
    Right tcs -> mapM_ toSpec tcs


-- | Point sets per color, Crosses form the solution
readInput    :: FilePath -> IO (Either ConversionError [TestCase Rational])
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
                            sameAsNaive segs

-- | Test if we have the same intersection points
samePointsAsNaive segs = it "Same points as Naive" $ do
  (Map.keys $ Sweep.intersections segs) `shouldBe` (Map.keys $ Naive.intersections segs)

-- | Test if they every intersection point has the right segments
sameAsNaive      :: (Fractional r, Ord r, Eq p
                    , Show p, Show r
                    ) => [LineSegment 2 p r] -> Spec
sameAsNaive segs = it "Same as Naive " $ do
    (Sweep.intersections segs) `shouldBe` (Naive.intersections segs)


data SelfIntersectionTestCase r = SITestCase { _siPolygon :: SimplePolygon () r
                                             , _isSelfIntersectiong :: Bool
                                             } deriving (Show,Eq)


siTestCases    :: FilePath -> Spec
siTestCases fp = (runIO $ readSiInput =<< getDataFileName fp) >>= \case
    Left e    -> it "reading SelfIntersection file" $
                   expectationFailure $ "Failed to read ipe file " ++ show e
    Right tcs -> mapM_ siToSpec tcs

-- | polygons are considered self intersecting when they are red
readSiInput    :: FilePath -> IO (Either ConversionError [SelfIntersectionTestCase Rational])
readSiInput fp = fmap f <$> readSinglePageFile fp
  where
    f page = [ SITestCase pg (isRed a)
             | pg :+ a <- polies
             ]
      where
        polies = readAll page -- read all simple polygons
        isRed ats = lookupAttr (Proxy :: Proxy Stroke) ats == Just (IpeColor (Named "red"))


siToSpec                   :: SelfIntersectionTestCase Rational -> Spec
siToSpec (SITestCase pg b) = it ("SelfIntersecting?: " <> take 50 (show pg)) $ do
                               hasSelfIntersections pg `shouldBe` b
