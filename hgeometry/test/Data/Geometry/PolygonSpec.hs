{-# LANGUAGE OverloadedStrings #-}
module Data.Geometry.PolygonSpec (spec) where

import           Algorithms.Geometry.LineSegmentIntersection
import           Control.Lens                                ((^.), (^..))
import           Control.Monad
import qualified Data.ByteString                             as BS
import qualified Data.CircularSeq                            as C
import           Data.Ext
import           Data.Geometry
import           Data.Geometry.Boundary
import           Data.Geometry.Ipe
import           Data.Geometry.Polygon                       (fromPoints)
import           Data.Proxy
import           Data.Serialize
import           Paths_hgeometry_test
import           System.IO.Unsafe
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Instances                   ()

{-# NOINLINE allSimplePolygons #-}
allSimplePolygons :: [SimplePolygon () Double]
allSimplePolygons = unsafePerformIO $ do
  inp <- BS.readFile =<< getDataFileName "data/polygons.simple"
  case decode inp of
    Left msg -> error msg
    Right pts -> pure $
      [ fromPoints [ ext (Point2 x y) | (x,y) <- lst ]
      | lst <- pts
      ]

{-# NOINLINE allMultiPolygons #-}
allMultiPolygons :: [MultiPolygon () Double]
allMultiPolygons = unsafePerformIO $ do
  inp <- BS.readFile =<< getDataFileName "data/polygons.multi"
  case decode inp of
    Left msg -> error msg
    Right pts -> pure $
      [ MultiPolygon (C.fromList [ ext (Point2 x y) | (x,y) <- boundary ])
          (map toSimple holes)
      | (boundary:holes) <- pts
      ]
  where
    toSimple lst = fromPoints [ ext (Point2 x y) | (x,y) <- lst ]

instance Fractional a => Arbitrary (SimplePolygon () a) where
  arbitrary = fmap realToFrac <$> elements allSimplePolygons

instance Fractional a => Arbitrary (MultiPolygon () a) where
  arbitrary = fmap realToFrac <$> elements allMultiPolygons

spec :: Spec
spec = do
  testCases "test/Data/Geometry/pointInPolygon.ipe"
  it "read . show = id (SimplePolygon)" $ do
    property $ \(pts :: C.CSeq (Point 2 Rational :+ ())) ->
      let p = SimplePolygon pts in
      read (show p) == p
  it "read . show = id (MultiPolygon)" $ do
    property $ \(pts :: C.CSeq (Point 2 Rational :+ ())) ->
      let p = MultiPolygon pts [SimplePolygon pts] in
      read (show p) == p
  it "valid polygons" $ do
    forM_ allSimplePolygons $ \poly -> do
      hasSelfIntersections poly `shouldBe` False
      isCounterClockwise poly `shouldBe` True


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
