{-# LANGUAGE OverloadedStrings          #-}
module Algorithms.Geometry.VisibilityPolygon.VisibilityPolygonSpec where

import  Data.Geometry.PolygonSpec()
import qualified Algorithms.Geometry.VisibilityPolygon.Lee as RotationalSweep
import           Algorithms.Geometry.VisibilityPolygon.Lee (toCombinatorial
                                                           , Definer
                                                           , StarShapedPolygon
                                                           )
import           Control.Lens
import qualified Data.Vector.Circular as CVec
import           Data.Ext
import           Data.Geometry.Interval
import           Data.Geometry.Ipe
import           Data.Geometry.LineSegment
import           Data.Geometry.Point
import           Data.Geometry.Polygon
import qualified Data.List as L
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import           Data.Proxy
import           Data.RealNumber.Rational
import           Data.Semigroup
import qualified Data.Set as Set
import           Debug.Trace
import           Test.Hspec
import           Test.QuickCheck
import           Test.Util

--------------------------------------------------------------------------------

type R = RealNumber 5
type P = Int
type E = (P,P)




spec :: Spec
spec = do
    describe "Testing Visibility Polygon" $ do
      it "small testPg" $
        sweep origin testPg `shouldBe` testPgAnswer
      it "spike easy" $
        sweep (Point2 356 704) spike `shouldBe` spikeEasy
      it "spike" $
        sweep querySpike spike `shouldBe` spikeAnswer
  where
    sweep q pg = toCombinatorial $ RotationalSweep.visibilityPolygon q pg


testPgAnswer :: CVec.CircularVector (Either P (P,E))
testPgAnswer = CVec.unsafeFromList [Left 1,Left 2,Right (2,(3,4)),Right (6,(3,4)),Left 6,Left 7,Left 8]

testPg :: SimplePolygon Int R
testPg = fromPoints [ Point2 3    1     :+ 1
                    , Point2 3    2     :+ 2
                    , Point2 4    2     :+ 3
                    , Point2 2    4     :+ 4
                    , Point2 (-1) 4     :+ 5
                    , Point2 1    2     :+ 6
                    , Point2 (-3) (-1)  :+ 7
                    , Point2 4    (-1)  :+ 8
                    ]



querySpike :: Point 2 R
querySpike = Point2 252 704

spike :: SimplePolygon Int R
spike = read "SimplePolygon [Point2 160 656 :+ 1,Point2 288 640 :+ 2,Point2 320 704 :+ 3,Point2 368 640 :+ 4,Point2 368 736 :+ 5,Point2 288 752 :+ 6,Point2 256 704 :+ 7,Point2 224 768 :+ 8]"

spikeEasy = CVec.unsafeFromList [Left 5,Left 6, Right (3,(6,7)), Left 3, Left 4]

spikeAnswer = CVec.unsafeFromList [Right (7,(2,3)), Left 7, Left 8,Left 1,Left 2]







--     ipeSpec

-- testPath = "test/Algorithms/Geometry/VisibilityPolygon/"

-- ipeSpec :: Spec
-- ipeSpec = testCases (testPath <> "manual.ipe")

-- testCases    :: FilePath -> Spec
-- testCases fp = (runIO $ readInput fp) >>= \case
--     Left e    -> it "reading VisibilityPolygon file" $
--                    expectationFailure $ "Failed to read ipe file " ++ show e
--     Right tcs -> mapM_ toSpec tcs


-- -- | Point sets per color, Crosses form the solution
-- readInput    :: FilePath -> IO (Either ConversionError [TestCase Rational])
-- readInput fp = fmap f <$> readSinglePageFile fp
--   where
--     f page = [TestCase segs]
--       where
--         segs = page^..content.traverse._IpePath.core._asLineSegment



-- data TestCase r = TestCase { _segments :: [LineSegment 2 () r]
--                            } deriving (Show,Eq)


-- toSpec                 :: (Fractional r, Ord r, Show r) => TestCase r -> Spec
-- toSpec (TestCase segs) = describe ("testing segments ") $ do
--                             samePointsAsNaive segs
--                             sameAsNaive segs

-- -- | Test if we have the same intersection points
-- samePointsAsNaive segs = it "Same points as Naive" $ do
--   (Map.keys $ Sweep.intersections segs) `shouldBe` (Map.keys $ Naive.intersections segs)

-- -- | Test if they every intersection point has the right segments
-- sameAsNaive      :: (Fractional r, Ord r, Eq p
--                     , Show p, Show r
--                     ) => [LineSegment 2 p r] -> Spec
-- sameAsNaive segs = it "Same as Naive " $ do
--     (Sweep.intersections segs) `shouldBe` (Naive.intersections segs)


-- data SelfIntersectionTestCase r = SITestCase { _siPolygon :: SimplePolygon () r
--                                              , _isSelfIntersectiong :: Bool
--                                              } deriving (Show,Eq)


-- siTestCases    :: FilePath -> Spec
-- siTestCases fp = (runIO $ readSiInput fp) >>= \case
--     Left e    -> it "reading SelfIntersection file" $
--                    expectationFailure $ "Failed to read ipe file " ++ show e
--     Right tcs -> mapM_ siToSpec tcs

-- -- | polygons are considered self intersecting when they are red
-- readSiInput    :: FilePath -> IO (Either ConversionError [SelfIntersectionTestCase Rational])
-- readSiInput fp = fmap f <$> readSinglePageFile fp
--   where
--     f page = [ SITestCase pg (isRed a)
--              | pg :+ a <- polies
--              ]
--       where
--         polies = page^..content.to flattenGroups.traverse
--                ._withAttrs _IpePath _asSimplePolygon
--         isRed ats = lookupAttr (Proxy :: Proxy Stroke) ats == Just (IpeColor (Named "red"))


-- siToSpec                   :: SelfIntersectionTestCase Rational -> Spec
-- siToSpec (SITestCase pg b) = it ("SelfIntersecting?: " <> take 50 (show pg)) $ do
--                                hasSelfIntersections pg `shouldBe` b



-- flattenGroups :: [IpeObject r] -> [IpeObject r]
-- flattenGroups = concatMap flattenGroups'

-- flattenGroups'                              :: IpeObject r -> [IpeObject r]
-- flattenGroups' (IpeGroup (Group gs :+ ats)) =
--       map (applyAts ats) . concatMap flattenGroups' $ gs
--     where
--       applyAts ats = id
-- flattenGroups' o                            = [o]

visibilityPg :: Point 2 R -> SimplePolygon () R -> StarShapedPolygon (Definer () ((),()) R) R
visibilityPg = RotationalSweep.visibilityPolygon

maiG = do poly <- generate arbitrary
          let outP = visibilityPg q poly
              q    = pickPoint poly
              out = singlePageFromContent [ iO $ defIO outP ! attr SFill (IpeColor "blue")
                                          , iO $ defIO poly
                                          , iO $ defIO q
                                          ]
          writeIpeFile "/tmp/out.ipe" out
