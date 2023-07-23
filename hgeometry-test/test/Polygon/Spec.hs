{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Polygon.Spec (spec) where

import           Algorithms.Geometry.LineSegmentIntersection
import           Control.Lens (over, view, (^.), (^..))
import           Control.Monad.Random (Random, evalRand, mkStdGen)
import qualified Data.ByteString as BS
import           Data.Coerce
import           Data.Double.Approximate
import           Data.Ext
import qualified Data.Foldable as F
import           Geometry
import           Geometry.Boundary
import           Ipe
import           Geometry.Polygon
import           Geometry.Polygon.Monotone
import           Geometry.Triangle
import           Data.Ord
import           Data.Proxy
import           Data.Ratio
import           Data.RealNumber.Rational
import           Data.Serialize
import qualified Data.Vector as V
import           Data.Vector.Circular (CircularVector)
import qualified Data.Vector.Circular as CV
import           Paths_hgeometry_test
import           System.IO.Unsafe
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()
import           Geometry.Transformation
import Data.Util
import Data.Maybe
import Data.Vinyl
import Data.Vinyl.CoRec
import Debug.Trace

type R = RealNumber 5


spec :: Spec
spec = do
  testCases "src/Geometry/pointInPolygon.ipe"

myPg :: SimplePolygon () R
myPg = read "SimplePolygon [Point2 7865790 3349116 :+ (),Point2 6304943 3123049 :+ (),Point2 5770988 3123102 :+ (),Point2 5770988 3123103 :+ (),Point2 5093248 2691560 :+ (),Point2 4456582 2321791 :+ (),Point2 3984237 1931429 :+ (),Point2 3327061 1479350 :+ (),Point2 2423390 1130062 :+ (),Point2 184830 842440 :+ (),Point2 0 410951 :+ (),Point2 1376016 61610 :+ (),Point2 3861016 0 :+ (),Point2 6058502 205475 :+ (),Point2 8030084 452025 :+ (),Point2 9734688 719111 :+ (),Point2 11357118 1047861 :+ (),Point2 11316045 1582088 :+ (),Point2 11192824 2034113 :+ (),Point2 10741016 2671078 :+ (),Point2 9447147 3123049 :+ ()]"

testCases    :: FilePath -> Spec
testCases fp = runIO (readInputFromFile =<< getDataFileName fp) >>= \case
    Left e    -> it "reading point in polygon file" $
                   expectationFailure $ "Failed to read ipe file " ++ show e
    Right tcs -> mapM_ toSpec tcs

data TestCase r = TestCase { _polygon    :: SimplePolygon () r
                           , _inside     :: [Point 2 r]
                           , _onBoundary :: [Point 2 r]
                           , _outside    :: [Point 2 r]
                           }
                  deriving (Show)


toSingleSpec poly r q = (q `inPolygon` poly) `shouldBe` r
  -- where
  --   msg = "Point in polygon test with " ++ show q


toSpec (TestCase poly is bs os) = do
                                    specify "inside tests" $
                                      mapM_ (toSingleSpec poly Inside) is
                                    specify "on boundary tests" $
                                      mapM_ (toSingleSpec poly OnBoundary) bs
                                    specify "outside tests" $
                                      mapM_ (toSingleSpec poly Outside) os

readInputFromFile    :: FilePath -> IO (Either ConversionError [TestCase Rational])
readInputFromFile fp = fmap f <$> readSinglePageFile fp
  where
    f page = [ TestCase poly
                        [ s^.symbolPoint | s <- myPoints ats, isInsidePt  s ]
                        [ s^.symbolPoint | s <- myPoints ats, isBorderPt  s ]
                        [ s^.symbolPoint | s <- myPoints ats, isOutsidePt s ]
             | (poly :+ ats) <- polies
             ]
      where
        polies = page^..content.traverse._withAttrs _IpePath _asSimplePolygon
        syms   = page^..content.traverse._IpeUse


        myPoints polyAts = [s | (s :+ ats) <- syms, belongsToPoly ats polyAts ]

        -- We test a point/polygon combination if they have the same color
        belongsToPoly symAts polyAts =
            lookupAttr colorP symAts == lookupAttr colorP polyAts

        -- A point i inside if it is a disk
        isInsidePt   :: IpeSymbol r -> Bool
        isInsidePt s = s^.symbolName == "mark/disk(sx)"

        -- Boxes are on the boundary
        isBorderPt s = s^.symbolName == "mark/box(sx)"

        -- crosses are outside the polygon
        isOutsidePt s = s^.symbolName == "mark/cross(sx)"

        colorP = Proxy :: Proxy Stroke




-- main = readInputFromFile "tests/Geometry/pointInPolygon.ipe"


----------------------------------
-- Numerical Robustness

-- Test case found by Kamil Figiela @kfigiela.
polygon :: (Eq r, Fractional r) => SimplePolygon () r
polygon = fromPoints $ map ext
  [ Point2 5584390.945938013 2284567.4635945037
  , Point2 5562410.061516319 2285869.7979417136
  , Point2 5563196.65161862  2250738.663576637
  , Point2 5579688.373487147 2252038.6420285213
  ]


insidePoint, outsidePoint :: Fractional r => Point 2 r
insidePoint  = Point2 5565974.538888888 2273030.9266712796
outsidePoint = Point2 5814191.399840455 2393283.2821864313

numericalSpec :: Spec
numericalSpec =
  describe "insidePolygon" $ do
    specify "baseline check" $ do
      ((insidePoint::Point 2 Rational) `inPolygon` polygon) `shouldBe` Inside
      ((outsidePoint::Point 2 Rational) `inPolygon` polygon) `shouldBe` Outside
    it "describes possible regression" $ do
      ((insidePoint::Point 2 Double) `inPolygon` polygon) `shouldBe` Inside
      ((outsidePoint::Point 2 Double) `inPolygon` polygon) `shouldBe` Outside
    it "describes possible regression" $ do
      ((insidePoint::Point 2 SafeDouble) `inPolygon` polygon) `shouldBe` Inside
      ((outsidePoint::Point 2 SafeDouble) `inPolygon` polygon) `shouldBe` Outside
