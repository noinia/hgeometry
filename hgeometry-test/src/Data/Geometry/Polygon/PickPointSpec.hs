{-# LANGUAGE OverloadedStrings #-}
module Data.Geometry.Polygon.PickPointSpec where

import Control.Lens
import Data.Ext
import Data.Geometry
import Data.Geometry.Boundary
import Data.Geometry.Polygon
import Ipe
import Paths_hgeometry_test
import Test.Hspec

--------------------------------------------------------------------------------

spec :: Spec
spec = do testCases "src/Data/Geometry/pointInPolygon.ipe"
          testCases "../hgeometry-examples/data/world.ipe"

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

-- main = readInputFromFile "tests/Data/Geometry/pointInPolygon.ipe"
