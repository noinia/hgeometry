module Algorithms.Geometry.LineSegmentIntersection.BentleyOttmannSpec where

import qualified Algorithms.Geometry.LineSegmentIntersection.BentleyOttmann as Sweep
import qualified Algorithms.Geometry.LineSegmentIntersection.Naive as Naive
import Algorithms.Geometry.LineSegmentIntersection.Types
import           Control.Lens
import           Data.Ext
import           Data.Geometry.Ipe
import           Data.Geometry.LineSegment
import           Data.Geometry.Interval
import           Data.Geometry.Point
import qualified Data.List as L
import qualified Data.List.NonEmpty as NonEmpty
import           Test.Hspec
import           Util
import qualified Data.Set as Set
import qualified Data.Map as Map



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
                            samePointsAsNaive segs
                            sameAsNaive segs


-- newtype S p r = S (IntersectionPoint p r)

-- instance (Eq p, Eq r) => Eq (S p r) where
--   (S p) == (S q) = diffBy samePoint (p^.) ys

samePoint ::  (Eq p, Eq r) => IntersectionPoint p r -> IntersectionPoint p r -> Bool
samePoint (IntersectionPoint p es is) (IntersectionPoint q xs ys) =
  p == q && (L.null $ xs L.\\ ys) && (L.null $ ys L.\\ xs)



-- sameAsNaive      :: (Fractional r, Ord r, Eq p
--                     , Show p, Show r
--                     ) => [LineSegment 2 p r] -> Spec
-- sameAsNaive segs = it "Same as Naive " $ do
--   -- sameIntersections
--   (Sweep.intersections segs) `shouldBe` (Naive.intersections segs)
--   -- `shouldBe` True

instance (Ord p, Ord r) => Ord (LineSegment 2 p r) where
  s `compare` t = (s^.start,s^.end) `compare` (s^.start,s^.end)


sameAsNaive      :: (Fractional r, Ord r, Ord p
                    , Show p, Show r
                    ) => [LineSegment 2 p r] -> Spec
sameAsNaive segs = it "Same as Naive " $ do
    (f $ Sweep.intersections segs) `shouldBe` (f $ Naive.intersections segs)
  where
    f = Map.fromList . map (\p -> (p^.intersectionPoint, ( Set.fromList $ p^.endPointOf
                                                         , Set.fromList $ p^.interiorTo
                                                         )))

samePointsAsNaive segs = it "Same points as Naive" $ do
  (f $ Sweep.intersections segs) `shouldBe` (f $ Naive.intersections segs)
   where
     f = Set.fromList . map (^.intersectionPoint)


  -- `shouldBe` True
