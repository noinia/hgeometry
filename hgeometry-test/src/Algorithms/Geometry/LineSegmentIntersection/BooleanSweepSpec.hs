{-# LANGUAGE OverloadedStrings #-}
module Algorithms.Geometry.LineSegmentIntersection.BooleanSweepSpec (spec) where

import           Algorithms.Geometry.LineSegmentIntersection (hasSelfIntersections)
import qualified Algorithms.Geometry.LineSegmentIntersection.BentleyOttmann as BO
import qualified Algorithms.Geometry.LineSegmentIntersection.BooleanSweep as Sweep
import qualified Algorithms.Geometry.LineSegmentIntersection.Naive as Naive
import           Data.Ext
import           Geometry.LineSegment
import           Geometry.Point
import           Geometry.Polygon
import           Data.Proxy
import           Data.RealNumber.Rational
import           Ipe
import           Paths_hgeometry_test
import           Test.Hspec
import           Test.QuickCheck

--------------------------------------------------------------------------------

testPath = "src/Algorithms/Geometry/LineSegmentIntersection/"

type R = RealNumber 5

spec :: Spec
spec = do
  describe "Self Intersecting Polygon Tests" $ do
    siTestCases (testPath <> "selfIntersections.ipe")

data SelfIntersectionTestCase r = SITestCase { _siPolygon :: SimplePolygon () r
                                             , _isSelfIntersectiong :: Bool
                                             } deriving (Show,Eq)


siTestCases    :: FilePath -> Spec
siTestCases fp = (runIO $ readSiInput =<< getDataFileName fp) >>= \case
    Left e    -> it "reading SelfIntersection file" $
                   expectationFailure $ "Failed to read ipe file " ++ show e
    Right tcs -> mapM_ siToSpec tcs

-- | polygons are considered self intersecting when they are red
readSiInput    :: FilePath -> IO (Either ConversionError [SelfIntersectionTestCase R])
readSiInput fp = fmap f <$> readSinglePageFile fp
  where
    f page = [ SITestCase pg (isRed a)
             | pg :+ a <- polies
             ]
      where
        polies = readAll page -- read all simple polygons
        isRed ats = lookupAttr (Proxy :: Proxy Stroke) ats == Just (IpeColor (Named "red"))


siToSpec                   :: SelfIntersectionTestCase R -> Spec
siToSpec (SITestCase pg b) = it ("SelfIntersecting?: " <> take 50 (show pg)) $ do
                               hasSelfIntersections pg `shouldBe` b
