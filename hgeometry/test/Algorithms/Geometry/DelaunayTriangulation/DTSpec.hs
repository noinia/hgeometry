module Algorithms.Geometry.DelaunayTriangulation.DTSpec where

import qualified Algorithms.Geometry.DelaunayTriangulation.DivideAndConquer as DC
import qualified Algorithms.Geometry.DelaunayTriangulation.Naive as Naive
import           Algorithms.Geometry.DelaunayTriangulation.Types
import           Control.Lens
import qualified Data.CircularList.Util as CU
import           Data.Ext
import           Data.Geometry
import           Data.Geometry.Ipe
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as M
import           Data.Maybe (mapMaybe, fromJust)
import qualified Data.Vector as V
import           Test.Hspec
import           Util


dtEdges :: (Fractional r, Ord r)
        => NonEmpty.NonEmpty (Point 2 r :+ p) -> [(VertexID, VertexID)]
dtEdges = tEdges . DC.delaunayTriangulation

take'   :: Int -> NonEmpty.NonEmpty a -> NonEmpty.NonEmpty a
take' i = NonEmpty.fromList . NonEmpty.take i

--------------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "Testing Divide and Conquer Algorithm for Delaunay Triangulation" $ do
    it "singleton " $ do
      dtEdges (take' 1 myPoints) `shouldBe` []
    toSpec (TestCase "myPoints" myPoints)
    toSpec (TestCase "myPoints'" myPoints')
    ipeSpec

ipeSpec :: Spec
ipeSpec = testCases "test/Algorithms/Geometry/SmallestEnclosingDisk/manual.ipe"

testCases    :: FilePath -> Spec
testCases fp = (runIO $ readInput fp) >>= \case
    Left e    -> it "reading Delaunay Triangulation disk file" $
                   expectationFailure $ "Failed to read ipe file " ++ show e
    Right tcs -> mapM_ toSpec tcs


-- | Point sets per color, Crosses form the solution
readInput    :: FilePath -> IO (Either ConversionError [TestCase Rational])
readInput fp = fmap f <$> readSinglePageFile fp
  where
    f page = [ TestCase "?" $ fmap (\p -> p^.core.symbolPoint :+ ()) pSet
             | pSet <- byStrokeColour' syms
             ]
      where
        syms = page^..content.traverse._IpeUse
        byStrokeColour' = mapMaybe NonEmpty.nonEmpty . byStrokeColour



data TestCase r = TestCase { _color    :: String
                           , _pointSet :: NonEmpty.NonEmpty (Point 2 r :+ ())
                           } deriving (Show,Eq)


toSpec                    :: (Fractional r, Ord r, Show r) => TestCase r -> Spec
toSpec (TestCase c pts) = describe ("testing on " ++ c ++ " points") $ do
                            sameAsNaive c pts

sameAsNaive       :: (Fractional r, Ord r, Show p, Show r)
                  => String -> NonEmpty.NonEmpty (Point 2 r :+ p) -> Spec
sameAsNaive s pts = it ("Divide And Conquer same answer as Naive on " ++ s) $
                      (Naive.delaunayTriangulation pts
                       `sameEdges`
                       DC.delaunayTriangulation pts) `shouldBe` True


sameEdges             :: Triangulation p r -> Triangulation p r -> Bool
triA `sameEdges` triB = all sameAdj . M.assocs $ mapping'
  where
    sameAdj (a, b) = (f $ adjA V.! a) `CU.isShiftOf` (adjB V.! b)

    adjA = triA^.neighbours
    adjB = triB^.neighbours

    mapping' = M.fromList $ zip (M.elems $ triA^.vertexIds) (M.elems $ triB^.vertexIds)

    f = fmap (fromJust . flip M.lookup mapping')

myPoints :: NonEmpty.NonEmpty (Point 2 Rational :+ ())
myPoints = NonEmpty.fromList . map ext $
           [ Point2 1  3
           , Point2 4  26
           , Point2 5  17
           , Point2 6  7
           , Point2 12 16
           , Point2 19 4
           , Point2 20 0
           , Point2 20 11
           , Point2 23 23
           , Point2 31 14
           , Point2 33 5
           ]

myPoints' :: NonEmpty.NonEmpty (Point 2 Rational :+ ())
myPoints' = NonEmpty.fromList . map ext $
            [ Point2 64  736
            , Point2 96 688
            , Point2 128 752
            , Point2 160 704
            , Point2 128 672
            , Point2 64 656
            , Point2 192 736
            , Point2 208 704
            , Point2 192 672
            ]
