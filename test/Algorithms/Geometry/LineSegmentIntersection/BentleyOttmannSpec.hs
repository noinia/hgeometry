module Algorithms.Geometry.LineSegmentIntersection.BentleyOttmannSpec where

import Data.Geometry.LineSegment
import Data.Ext
import Data.Geometry.Point
import Control.Lens
import qualified Algorithms.Geometry.LineSegmentIntersection.BentleyOttmann as Sweep


import           Test.Hspec
import           Util
import           Data.Geometry.Ipe
import qualified Data.List.NonEmpty as NonEmpty



spec :: Spec
spec = do
  describe "Testing Bentley Ottmann LineSegment Intersection" $ do
    -- toSpec (TestCase "myPoints" myPoints)
    -- toSpec (TestCase "myPoints'" myPoints')
    ipeSpec

ipeSpec :: Spec
ipeSpec = testCases "test/Algorithms/Geometry/LineSegmentIntersection/manual.ipe"

testCases    :: FilePath -> Spec
testCases fp = (runIO $ readInput fp) >>= \case
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
                            sameAsNaive segs


sameAsNaive segs = it "fails " $ do
  Sweep.intersections segs `shouldBe` []
