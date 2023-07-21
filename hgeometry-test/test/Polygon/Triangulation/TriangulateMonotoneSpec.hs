{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes #-}
module Polygon.Triangulation.TriangulateMonotoneSpec (spec) where

import Control.Lens
import Data.Maybe
import Golden
import HGeometry
import HGeometry.Ext
import HGeometry.Number.Real.Rational
import HGeometry.Polygon.Simple
import HGeometry.Polygon.Triangulation.TriangulateMonotone
import Ipe
import System.OsPath
import Test.Hspec
import Test.Util

--------------------------------------------------------------------------------

type R = RealNumber 5


spec :: Spec
spec = do testCases [osp|Polygon/Triangulation/monotone.ipe|]
          testCases [osp|Polygon/Triangulation/simplepolygon6.ipe|]

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
toSpec (TestCase (poly :+ c) sol) =
    describe ("testing polygions of color " ++ show c) $ do
      it "comparing with manual solution" $ do
        let algSol = computeDiagonals poly
        (naiveSet . map (over allPoints (view core)) $ algSol) `shouldBe` naiveSet sol
  where
    naiveSet = NaiveSet . map S

newtype S r = S (ClosedLineSegment (Point 2 r)) deriving (Show)

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
