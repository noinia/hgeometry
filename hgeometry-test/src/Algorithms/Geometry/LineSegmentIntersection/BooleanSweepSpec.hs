{-# LANGUAGE OverloadedStrings #-}
module Algorithms.Geometry.LineSegmentIntersection.BooleanSweepSpec (spec) where

import           Algorithms.Geometry.LineSegmentIntersection (hasSelfIntersections)
import qualified Algorithms.Geometry.LineSegmentIntersection.BentleyOttmann as BO
import qualified Algorithms.Geometry.LineSegmentIntersection.BooleanSweep as Sweep
import qualified Algorithms.Geometry.LineSegmentIntersection.Naive as Naive
import           Data.Ext
import           Data.Geometry.LineSegment
import           Data.Geometry.Point
import           Data.Geometry.Polygon
import           Data.Proxy
import           Data.RealNumber.Rational
import           Ipe
import           Paths_hgeometry_test
import           Test.Hspec
import           Test.QuickCheck

--------------------------------------------------------------------------------

testPath = "src/Algorithms/Geometry/LineSegmentIntersection/"

type R = RealNumber 5

open,closed :: R -> R -> R -> R -> LineSegment 2 () R
open x1 y1 x2 y2 = OpenLineSegment (ext $ Point2 x1 y1) (ext $ Point2 x2 y2)
closed x1 y1 x2 y2 = ClosedLineSegment (ext $ Point2 x1 y1) (ext $ Point2 x2 y2)

line1  = closed 0 0       1 0
line2  = closed 0 0       0 1

line3  = open   0 0       1 0
line4  = open   0 0       0 1

line5  = closed 0 0       1 1
line6  = open   0.5 0.5   0.5 0
line7  = closed 0.5 0.5   0.5 0

line8  = closed 10 10     0 0
line9  = closed 1  10     0 0
line10 = closed 10  1     0 0

line8'  = open  10 10     0 0
line9'  = open  1 10      0 0
line10' = open  10 1      0 0

line11 = closed 0 0       2 0
line12 = closed 1 0       3 0

line13 = open   0 0       2 0
line14 = open   2 0       4 0

spec :: Spec
spec = do
  it "doesn't overlap" $
    Sweep.hasIntersections [] `shouldBe` False
  it "end-point overlap" $
    Sweep.hasIntersections [line1, line2] `shouldBe` True
  it "open endpoints" $
    Sweep.hasIntersections [line3, line4] `shouldBe` False
  it "open endpoints / midpoint" $
    Sweep.hasIntersections [line5, line6] `shouldBe` False
  it "closed endpoints / midpoint" $
    Sweep.hasIntersections [line5, line7] `shouldBe` True
  it "same endpoint" $
    Sweep.hasIntersections [line8, line9, line10] `shouldBe` True
  it "same endpoint / open" $
    Sweep.hasIntersections [line8', line9', line10'] `shouldBe` False
  it "horizontal overlap" $
    Sweep.hasIntersections [line11, line12] `shouldBe` True
  it "horizontal near overlap" $
    Sweep.hasIntersections [line13, line14] `shouldBe` False
  it "matches naive" $
    property $ \(lst :: [LineSegment 2 () R]) ->
      Sweep.hasIntersections lst === (not . null . BO.interiorIntersections $ lst)
  it "matches boolean hasInterior" $
    property $ \(lst :: [LineSegment 2 () R]) ->
      Sweep.hasIntersections lst === not (null $ Naive.intersections lst)
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
