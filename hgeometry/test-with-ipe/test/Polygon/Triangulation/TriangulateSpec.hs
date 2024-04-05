{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes #-}
module Polygon.Triangulation.TriangulateSpec (spec) where

import Control.Lens
import HGeometry
import HGeometry.Ext
import HGeometry.Number.Real.Rational
import HGeometry.PlaneGraph
-- import HGeometry.Polygon.Instances ()
import HGeometry.Polygon.Triangulation
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Instances ()

--------------------------------------------------------------------------------

-- type R = RealNumber 5

spec :: Spec
spec = pure ()

-- spec :: Spec
-- spec = do testCases [osp|test-with-ipe/Polygon/Triangulation/monotone.ipe|]
--           -- testCases [osp|test-with-ipe/Polygon/Triangulation/simplepolygon6.ipe|]

-- testCases    :: OsPath -> Spec
-- testCases fp = (runIO $ readInput =<< getDataFileName fp) >>= \case
--     Left e    -> it "reading TriangulateMonotone file" $
--                    expectationFailure $ "Failed to read ipe file " ++ show e
--     Right tcs -> mapM_ toSpec tcs

-- data TestCase r = TestCase { _polygon  :: SimplePolygon (Point 2 r) :+ IpeColor r
--                            , _solution :: [ClosedLineSegment (Point 2 r)]
--                            }
--                   deriving (Show,Eq)

-- toSpec                            :: (Num r, Ord r, Show r) => TestCase r -> Spec
-- toSpec (TestCase (poly :+ c) sol) =
--     describe ("testing polygions of color " ++ show c) $ do
--       it "comparing with manual solution" $ do
--         let algSol = triangulate @() poly
--         (naiveSet . map toSeg $ algSol) `shouldBe` naiveSet sol
