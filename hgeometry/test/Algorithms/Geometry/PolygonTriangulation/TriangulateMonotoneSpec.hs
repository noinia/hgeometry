{-# LANGUAGE OverloadedStrings          #-}
module Algorithms.Geometry.PolygonTriangulation.TriangulateMonotoneSpec where

import           Algorithms.Geometry.PolygonTriangulation.TriangulateMonotone
import           Control.Lens
import           Data.Ext
import           Data.Geometry
import           Data.Geometry.Ipe
import           Data.Geometry.Polygon
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Maybe
import qualified Data.Set as Set
import           Data.Vinyl
import           Test.Hspec
import           Test.Util


spec :: Spec
spec = do testCases "test/Algorithms/Geometry/PolygonTriangulation/monotone.ipe"
          testCases "test/Algorithms/Geometry/PolygonTriangulation/simplepolygon6.ipe"

testCases    :: FilePath -> Spec
testCases fp = (runIO $ readInput fp) >>= \case
    Left e    -> it "reading TriangulateMonotone file" $
                   expectationFailure $ "Failed to read ipe file " ++ show e
    Right tcs -> mapM_ toSpec tcs


data TestCase r = TestCase { _polygon  :: MonotonePolygon () r :+ IpeColor r
                           , _solution :: [LineSegment 2 () r]
                           }
                  deriving (Show,Eq)


toSpec                            :: (Num r, Ord r, Show r) => TestCase r -> Spec
toSpec (TestCase (poly :+ c) sol) =
    describe ("testing polygions of color " ++ show c) $ do
      it "comparing with manual solution" $
        (naiveSet $ computeDiagonals poly) `shouldBe` naiveSet sol
  where
    naiveSet = NaiveSet . map S

newtype S p r = S (LineSegment 2 p r) deriving (Show)

instance (Eq p, Eq r) => Eq (S p r) where
  (S s) == (S y) =  (s^.start == y^.start && s^.end == y^.end)
                 || (s^.start == y^.end   && s^.end == y^.start)



-- | Point sets per color, Crosses form the solution
readInput    :: FilePath -> IO (Either ConversionError [TestCase Rational])
readInput fp = fmap f <$> readSinglePageFile fp
  where
    f page = [ TestCase pg (solutionOf pg segs)
             | pg <- map g polies
             ]
      where
        g x@(pg :+ _) = toCounterClockWiseOrder pg :+ lookupColor x

        polies = page^..content.traverse._withAttrs _IpePath _asSimplePolygon
        segs   = page^..content.traverse._withAttrs _IpePath _asLineSegment

        solutionOf (_ :+ c) = map (^.core) . filter ((== c) . lookupColor)


        -- -- | Crosses form a solution
        -- isInSolution s = s^.core.symbolName == "mark/cross(sx)"

        -- right = either (const Nothing) Just
        -- solutionOf = right . fromList . map (^.core.symbolPoint) . filter isInSolution

lookupColor           :: i :+ IpeAttributes Path r -> IpeColor r
lookupColor (_ :+ ats) = fromMaybe (IpeColor $ Named "black") $ lookupAttr SStroke ats
