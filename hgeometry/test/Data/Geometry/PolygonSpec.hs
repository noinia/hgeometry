{-# LANGUAGE OverloadedStrings #-}
module Data.Geometry.PolygonSpec where

import Data.Traversable(traverse)
import Data.Ext
import Control.Lens
import Control.Applicative
import Data.Geometry
import Data.Geometry.Boundary
import Data.Geometry.Ipe
import Data.Proxy
import Test.Hspec

spec :: Spec
spec = testCases "test/Data/Geometry/pointInPolygon.ipe"


testCases    :: FilePath -> Spec
testCases fp = runIO (readInputFromFile fp) >>= \case
    Left e    -> it "reading point in polygon file" $
                   expectationFailure $ "Failed to read ipe file " ++ show e
    Right tcs -> mapM_ toSpec tcs


--   ipeF <- beforeAll $ readInputFromFile "tests/Data/Geometry/pointInPolygon.ipe"
--   describe "Point in Polygon tests" $ do
--     it "returns the first element of a list" $ do
--       head [23 ..] `shouldBe` (23 :: Int)


data TestCase r = TestCase { _polygon    :: SimplePolygon () r
                           , _inside     :: [Point 2 r]
                           , _onBoundary :: [Point 2 r]
                           , _outside    :: [Point 2 r]
                           }
                  deriving (Show)


toSingleSpec poly r q = it msg $ (q `inPolygon` poly) `shouldBe` r
  where
    msg = "Point in polygon test with " ++ show q


toSpec (TestCase poly is bs os) = do
                                    describe "inside tests" $
                                      mapM_ (toSingleSpec poly Inside) is
                                    describe "on boundary tests" $
                                      mapM_ (toSingleSpec poly OnBoundary) bs
                                    describe "outside tests" $
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




-- main = readInputFromFile "tests/Data/Geometry/pointInPolygon.ipe"
