{-# LANGUAGE OverloadedStrings #-}
module Data.Geometry.TriangleSpec where

import Data.Traversable(traverse)
import Data.Ext
import Control.Lens
import Control.Applicative
import Data.Geometry
import Data.Geometry.Triangle
import Data.Geometry.Boundary
import Data.Geometry.Ipe
import Data.Proxy
import Test.Hspec
import Data.Ratio
import Data.Vinyl.CoRec

spec :: Spec
spec = do testCases "test/Data/Geometry/pointInTriangle.ipe"
          describe "intersection tests" $ do
            it "intersecting Line 2 with Triangle 2 " $ do
              let t :: Triangle 2 () Rational
                  t = Triangle (ext origin) (ext $ Point2 10 0) (ext $ Point2 10 10)
                  hor :: Rational -> Line 2 Rational
                  hor = horizontalLine
              (hor 3 `intersect` t)
                `shouldBe` (coRec $ ClosedLineSegment (ext $ Point2 10 (3 :: Rational))
                                                      (ext $ Point2 3  (3 :: Rational)))
              (hor 10 `intersect` t)
                `shouldBe` (coRec $ Point2 10 (10 :: Rational))
              (hor 11 `intersect` t)
                `shouldBe` (coRec NoIntersection)


testCases    :: FilePath -> Spec
testCases fp = runIO (readInputFromFile fp) >>= \case
    Left e    -> it "reading point in triangle file" $
                   expectationFailure $ "Failed to read ipe file " ++ show e
    Right tcs -> mapM_ toSpec tcs


--   ipeF <- beforeAll $ readInputFromFile "tests/Data/Geometry/pointInPolygon.ipe"
--   describe "Point in Polygon tests" $ do
--     it "returns the first element of a list" $ do
--       head [23 ..] `shouldBe` (23 :: Int)


data TestCase r = TestCase { _triangle   :: Triangle 2 () r
                           , _inside     :: [Point 2 r]
                           , _onBoundary :: [Point 2 r]
                           , _outside    :: [Point 2 r]
                           }
                  deriving (Show)


toSingleSpec poly r q = it msg $ (q `inTriangle` poly) `shouldBe` r
  where
    msg = "Point in triangle test with " ++ show q


toSpec                          ::  (Show r, Ord r, Fractional r)
                                =>  TestCase r -> Spec
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
        polies = page^..content.traverse._withAttrs _IpePath _asTriangle
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
