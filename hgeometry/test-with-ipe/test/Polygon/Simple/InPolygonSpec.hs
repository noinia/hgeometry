{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Polygon.Simple.InPolygonSpec (spec) where

import           Control.Lens
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Proxy
import           Golden
import           HGeometry.Boundary
import           HGeometry.Ext
import           HGeometry.Point
import           HGeometry.Polygon.Simple
import           Ipe
import           System.OsPath
import           Test.Hspec
import           Test.QuickCheck.Instances ()

--------------------------------------------------------------------------------

-- type R = RealNumber 5

spec :: Spec
spec = do
  testCases [osp|test-with-ipe/Polygon/Simple/pointInPolygon.ipe|]
  numericalSpec

testCases    :: OsPath -> Spec
testCases fp = runIO (readInputFromFile =<< getDataFileName fp) >>= \case
    Left e    -> it "reading point in polygon file" $
                   expectationFailure $ "Failed to read ipe file " ++ show e
    Right tcs -> mapM_ toSpec tcs

data TestCase r = TestCase { _polygon    :: SimplePolygon (Point 2 r)
                           , _inside     :: [Point 2 r]
                           , _onBoundary :: [Point 2 r]
                           , _outside    :: [Point 2 r]
                           }
                  deriving (Show)


toSingleSpec          :: (Fractional r, Ord r, Show r)
                      => SimplePolygon (Point 2 r)
                      -> PointLocationResult
                      -> Point 2 r
                      -> Spec
toSingleSpec poly r q = it name $ (asPointLocationResult $ q `inPolygon` poly) `shouldBe` r
  where
    name = unwords ["query:", show q, "in", take 70 $ show poly ]

toSpec (TestCase poly is bs os) = do
                                    describe "inside tests" $
                                      mapM_ (toSingleSpec poly Inside) is
                                    describe "on boundary tests" $
                                      mapM_ (toSingleSpec poly OnBoundary) bs
                                    describe "outside tests" $
                                      mapM_ (toSingleSpec poly Outside) os

readInputFromFile    :: OsPath -> IO (Either ConversionError [TestCase Rational])
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
polygon :: (Eq r, Fractional r) => SimplePolygon (Point 2 r)
polygon = uncheckedFromCCWPoints . NonEmpty.fromList $
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
      ((insidePoint::Point 2 Rational) `inPolygon` polygon) `shouldBe` StrictlyInside
      ((outsidePoint::Point 2 Rational) `inPolygon` polygon) `shouldBe` StrictlyOutside
    it "describes possible regression" $ do
      ((insidePoint::Point 2 Double) `inPolygon` polygon) `shouldBe` StrictlyInside
      ((outsidePoint::Point 2 Double) `inPolygon` polygon) `shouldBe` StrictlyOutside
    -- it "describes possible regression" $ do
    --   ((insidePoint::Point 2 SafeDouble) `inPolygon` polygon) `shouldBe` Inside
    --   ((outsidePoint::Point 2 SafeDouble) `inPolygon` polygon) `shouldBe` Outside
