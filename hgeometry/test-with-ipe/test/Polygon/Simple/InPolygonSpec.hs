{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Polygon.Simple.InPolygonSpec where -- (spec) where

import           Control.Lens
import           Control.Monad
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Maybe (fromJust)
import           Data.Proxy
import           Golden
import           HGeometry.Boundary
import           HGeometry.Ext
import           HGeometry.LineSegment
import           HGeometry.Number.Real.Rational
import           HGeometry.Point
import           HGeometry.Polygon.Class
import           HGeometry.Polygon.Simple
import           Ipe
import           System.OsPath
import           Test.Hspec
import           Test.QuickCheck.Instances ()

import           Debug.Trace
import           HGeometry.Intersection
import           HGeometry.Interval
import           HGeometry.Polygon.Simple.InPolygon
--------------------------------------------------------------------------------

type R = RealNumber 5

spec :: Spec
spec = do
  testCases [osp|test-with-ipe/Polygon/Simple/pointInPolygon.ipe|]
  numericalSpec
  lineSegmentContainsSpec

  it "darkOrangePolygonBug" $
    ((Point2 400 288) `inPolygon` darkOrangePoly) `shouldBe` StrictlyInside
  it "darkOrangePolygonBug2" $
    ((Point2 400 288) `inSimplePolygon` darkOrangePoly) `shouldBe` StrictlyInside
  it "darkOrangePolygonBug3" $
    ((Point2 400 240) `inSimplePolygon` darkOrangePoly) `shouldBe` StrictlyInside


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
toSingleSpec poly r q = it name $ (asPointLocationResult $ q `inSimplePolygon` poly) `shouldBe` r
  where
    name = unwords ["query:", show q, "in", take 70 $ show poly ]

toSpec (TestCase poly is bs os) = do
                                    describe "inside tests" $
                                      mapM_ (toSingleSpec poly Inside) is
                                    describe "on boundary tests" $
                                      mapM_ (toSingleSpec poly OnBoundary) bs
                                    describe "outside tests" $
                                      mapM_ (toSingleSpec poly Outside) os

readInputFromFile    :: OsPath -> IO (Either ConversionError [TestCase R])
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


--------------------------------------------------------------------------------
-- * Line segment inside polygon tests



lineSegmentContainsSpec :: Spec
lineSegmentContainsSpec = describe "containedIn tests" $ do
      (segs, polies) <-  runIO $ do
        inFp'      <- getDataFileName
                          ([osp|test-with-ipe/Polygon/Simple/segmentContainedInPolygon.ipe|])
        Right page <- readSinglePageFile inFp'
        let (segs' :: NonEmpty (ClosedLineSegment (Point 2 R) :+ _))
                     = NonEmpty.fromList $ readAll page
            (pgs'  :: NonEmpty (SimplePolygon (Point 2 R) :+ _))
                     = NonEmpty.fromList $ readAll page
        pure (segs',pgs')
      forM_ polies $ \(poly :+ ats) -> do
        describe ("containedIn polygon of color" <> show (fromJust $ lookupAttr SStroke ats)) $ do
          let (inSegs,outSegs) = NonEmpty.partition (sameColor ats) segs
            -- ClosedLineSegment (Point2 368 288) (Point2 400 288)

          describe "segments inside" $ do
            forM_ inSegs $ \(seg :+ _) -> do
              describe (show seg) $ do
                it "segment stats inside" $
                  ((seg^.start) `inPolygon` poly) `shouldNotBe` StrictlyOutside
                it "segment ends inside" $
                  ((seg^.end) `inPolygon` poly) `shouldNotBe` StrictlyOutside
                it "segments contained" $
                  seg `shouldSatisfy` (`containedIn` poly)
          it "segments ouutside" $ do
            forM_ outSegs $ \(seg :+ _) ->
              seg `shouldSatisfy` (not . (`containedIn` poly))

  where
    sameColor ats (_ :+ ats') = lookupAttr SStroke ats == lookupAttr SStroke ats'


darkOrangePoly :: SimplePolygon (Point 2 R)
darkOrangePoly = fromJust . fromPoints . NonEmpty.fromList $
  [ Point2 336 272
  , Point2 384 320
  , Point2 480 224
  , Point2 448 192
  , Point2 336 240
  , Point2 400 256
  ]
