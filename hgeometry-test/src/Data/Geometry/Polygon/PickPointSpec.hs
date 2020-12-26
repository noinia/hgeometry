{-# LANGUAGE OverloadedStrings #-}
module Data.Geometry.Polygon.PickPointSpec where

import Data.Ext
import Control.Lens
import Data.Geometry
import Data.Geometry.Polygon
import Data.Geometry.Boundary
import Data.Geometry.Ipe
import Test.Hspec
import           Paths_hgeometry_test

--------------------------------------------------------------------------------

spec :: Spec
spec = do testCases "src/Data/Geometry/pointInPolygon.ipe"
          testCases "../hgeometry-examples/data/world.ipe"
          it "Pick point in polygon test"  $
            toSpec testPoly
          it "Pick point in polygon test"  $
            toSpec testPoly2

testCases    :: FilePath -> Spec
testCases fp = runIO (readInputFromFile =<< getDataFileName fp) >>= \case
    Left e    -> it "reading point in polygon file" $
                   expectationFailure $ "Failed to read ipe file " ++ show e
    Right tcs -> it "Pick point in polygon test"  $
      mapM_ toSpec tcs

data TestCase r = TestCase { _polygon    :: SimplePolygon () r }
                  deriving (Show)

toSpec                  :: TestCase Rational -> Expectation
toSpec (TestCase  poly) = (pickPoint poly `inPolygon` poly) `shouldBe` Inside

readInputFromFile    :: FilePath -> IO (Either ConversionError [TestCase Rational])
readInputFromFile fp = fmap f <$> readSinglePageFile fp
  where
    f page = [ TestCase (toCounterClockWiseOrder poly) | (poly :+ _) <- polies ]
      where
        polies = page^..content.traverse._withAttrs _IpePath _asSimplePolygon

testPoly :: TestCase Rational
testPoly = TestCase . toCounterClockWiseOrder. fromPoints . map ext
         $ [origin, Point2 10 10, Point2 20 5, Point2 30 2, Point2 3 1, Point2 31 0]

testPoly2 :: TestCase Rational
testPoly2 = TestCase . toCounterClockWiseOrder. fromPoints . map ext
          $ [ Point2 208 752
            , Point2 304 688
            , Point2 224 592
            , Point2 48 736
            ]

-- main = readInputFromFile "tests/Data/Geometry/pointInPolygon.ipe"
