{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes #-}
module Polygon.Triangulation.TriangulateMonotoneSpec (spec) where

import           Control.Lens
import           Data.Maybe
import           Golden
import           HGeometry
import           HGeometry.Ext
import           HGeometry.Number.Real.Rational
import           HGeometry.Polygon.Class
import           HGeometry.Polygon.Simple
import           HGeometry.PlaneGraph
import           HGeometry.Polygon.Triangulation
import           Ipe
import           System.OsPath
import           Test.Hspec
import           Test.Util

import qualified HGeometry.Polygon.Triangulation.TriangulateMonotone as TM

import           Debug.Trace
--------------------------------------------------------------------------------

type R = RealNumber 5


spec :: Spec
spec = do it "triangulate triangle"   $ do
            let tri :: SimplePolygon (Point 2 R)
                tri = uncheckedFromCCWPoints $ [origin, Point2 10 0, Point2 10 10]
                gr  = TM.triangulate @() tri
            extractDiagonals gr `shouldBe` []
          testCases [osp|test-with-ipe/Polygon/Triangulation/monotone.ipe|]
          -- testCases [osp|test-with-ipe/Polygon/Triangulation/simplepolygon6.ipe|]

testCases    :: OsPath -> Spec
testCases fp = (runIO $ readInput =<< getDataFileName fp) >>= \case
    Left e    -> it "reading TriangulateMonotone file" $
                   expectationFailure $ "Failed to read ipe file " ++ show e
    Right tcs -> mapM_ toSpec tcs


data TestCase r = TestCase { _polygon  :: SimplePolygon (Point 2 r) :+ IpeColor r
                           , _solution :: [ClosedLineSegment (Point 2 r)]
                           }
                  deriving (Show,Eq)


toSpec                            :: (Num r, Ord r, Show r) => TestCase r -> Spec
toSpec (TestCase (poly :+ c) sol) = do
    describe ("testing polygons of color " ++ show c) $ do
      it "comparing monotone diagonals with manual solution" $ do
        let algSol = TM.computeDiagonals poly
        (naiveSet . map toSeg $ algSol) `shouldBe` naiveSet sol
      -- it "comparing diagonals with manual solution" $ do
      --   let algSol = computeDiagonals poly
      --   (naiveSet . map toSeg $ algSol) `shouldBe` naiveSet sol

      it "comparing diagonals from the triangualtion with manual solution" $ do
        let gr     = TM.triangulate @() poly
            algSol = traceShow ("edges", gr^..edges.withIndex)
                   $ extractDiagonals gr
        (naiveSet algSol) `shouldBe` naiveSet sol
  where
    naiveSet = NaiveSet . map S
    toSeg (Vector2 i j) = ClosedLineSegment (poly^?!vertexAt i) (poly^?!vertexAt j)





extractDiagonals gr = mapMaybe (\(d,edgeType) -> case edgeType of
                                   Diagonal -> gr^?edgeSegmentAt d
                                   _        -> Nothing
                               ) $ gr^..edges.withIndex


newtype S r = S (ClosedLineSegment (Point 2 r)) deriving newtype (Show)

instance (Eq r) => Eq (S r) where
  (S s) == (S y) =  (s^.start == y^.start && s^.end == y^.end)
                 || (s^.start == y^.end   && s^.end == y^.start)


-- | Point sets per color, Crosses form the solution
readInput    :: OsPath -> IO (Either ConversionError [TestCase R])
readInput fp = fmap f <$> readSinglePageFile fp
  where
    f page = [ TestCase pg (solutionOf pg segs)
             | pg <- map g polies
             ]
      where
        g x@(pg :+ _) = pg :+ lookupColor x

        polies = page^..content.traverse._withAttrs _IpePath _asSimplePolygon
        segs   = page^..content.traverse._withAttrs _IpePath _asClosedLineSegment

        solutionOf (_ :+ c) = map (^.core) . filter ((== c) . lookupColor)


        -- -- | Crosses form a solution
        -- isInSolution s = s^.core.symbolName == "mark/cross(sx)"

        -- right = either (const Nothing) Just
        -- solutionOf = right . fromList . map (^.core.symbolPoint) . filter isInSolution

lookupColor           :: i :+ IpeAttributes Path r -> IpeColor r
lookupColor (_ :+ ats) = fromMaybe (IpeColor $ Named "black") $ lookupAttr SStroke ats
