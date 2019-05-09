{-# LANGUAGE OverloadedStrings #-}
module Data.Geometry.Polygon.StarShapedSpec where

import Control.Lens
import Control.Monad.Random.Strict(evalRand)
import Data.Ext
import Data.Geometry
import Data.Geometry.Ipe
import Data.Geometry.Ipe.Color (named)
import Data.Maybe
import System.Random (mkStdGen)
import Test.Hspec

--------------------------------------------------------------------------------

spec :: Spec
spec = do testCases "test/Data/Geometry/Polygon/star_shaped.ipe"

testCases    :: FilePath -> Spec
testCases fp = runIO (readInputFromFile fp) >>= \case
    Left e    -> it "reading star-shaped file" $
                   expectationFailure $ "Failed to read ipe file " ++ show e
    Right tcs -> mapM_ toSpec tcs

data TestCase r = TestCase { _polygon    :: SimplePolygon () r
                           , _isStar     :: Bool
                           }
                  deriving (Show)

toSpec                   :: TestCase Rational -> Spec
toSpec (TestCase poly b) = it "isStarShaped test" $ do
                             -- correct result
                             isJust mq `shouldBe` b
                             -- it "returned point in polygon" $
                             (maybe True (`intersects` poly) mq) `shouldBe` True
  where
    mq = flip evalRand (mkStdGen 15)
           (isStarShaped poly)

readInputFromFile    :: FilePath -> IO (Either ConversionError [TestCase Rational])
readInputFromFile fp = fmap f <$> readSinglePageFile fp
  where
    -- star shaped polygons are blue
    f page = [ TestCase poly (isBlue att) | (poly :+ att) <- polies ]
      where
        polies = page^..content.traverse._withAttrs _IpePath _asSimplePolygon
        isBlue = maybe False (== named "blue") . lookupAttr SStroke


-- testPoly :: TestCase Rational
-- testPoly = TestCase . toCounterClockWiseOrder. fromPoints . map ext
--          $ [origin, Point2 10 10, Point2 20 5, Point2 30 2, Point2 3 1, Point2 31 0]

-- testPoly2 :: TestCase Rational
-- testPoly2 = TestCase . toCounterClockWiseOrder. fromPoints . map ext
--           $ [ Point2 208 752
--             , Point2 304 688
--             , Point2 224 592
--             , Point2 48 736
--             ]

-- -- main = readInputFromFile "tests/Data/Geometry/pointInPolygon.ipe"
