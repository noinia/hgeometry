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
    ((Point2 400 288) `inSimplePolygonX` darkOrangePoly) `shouldBe` StrictlyInside
  it "darkOrangePolygonBug3" $
    ((Point2 400 240) `inSimplePolygonX` darkOrangePoly) `shouldBe` StrictlyInside


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


--------------------------------------------------------------------------------



inSimplePolygonX        :: forall queryPoint simplePolygon point r.
                          ( Num r, Ord r, Point_ point 2 r, Point_ queryPoint 2 r
                          , SimplePolygon_ simplePolygon point r
                          , Show r, Show queryPoint, Show point
                          )
                       => queryPoint -> simplePolygon
                       -> PointLocationResultWith (VertexIx simplePolygon)
q `inSimplePolygonX` pg = case ifoldMapOf outerBoundaryWithNeighbours countAbove pg of
                            OnEdge s                       -> OnBoundaryEdge s
                            NumStrictlyAbove m | odd m     -> StrictlyInside
                                               | otherwise -> StrictlyOutside
  where
    -- we count the number of by the vertical upward ray from q intersects the boundary of
    -- the polygon. If the number of times we intersect the boundary is odd we are inside,
    -- and outside othwerise.

    --
    -- Generally, countAbove will compute the contribution of the edge uv (which is edge i).
    --
    --
    -- we have to take special care of vertical edges, and when the ray goes through a
    -- vertex u.
    countAbove (i,_) (u,(p,v)) = case (u^.xCoord) `compare` (q^.xCoord) of
      LT | (q^.xCoord) < (v^.xCoord) -> belowLineSeg i u v
         -- for q to lie below the edge, v has to lie right of q and q has to actually lie
         -- below the line segment.
         --
         -- Note that if q lies strictly below v we don't count it here. We handle it
         -- when handing vertex v
         | otherwise                 -> mempty

      GT | (q^.xCoord) > (v^.xCoord) -> belowLineSeg i v u
            -- for q to lie below the edge, v has to lie left of q and
            -- q has to actually lie below the line through u and v.
         | otherwise                 -> mempty


      EQ -> case (u^.yCoord) `compare` (q^.yCoord) of
              EQ                             -> OnEdge i
                -- q == u, so it lies on edge i

              LT | (q^.xCoord) == (v^.xCoord) &&
                   (q^.yCoord) < (v^.yCoord) -> OnEdge i
                 -- q lies above u. So the only case in which q does lie on the edge uv
                 -- is if it is vertical, and q lies on it.
                 | otherwise                 -> mempty
                 -- q lies above u, so otherwise it does not lie on the edge starting at u.

              GT -> case (q^.xCoord) `compare` (v^.xCoord) of
                EQ | (q^.yCoord) > (v^.yCoord) -> OnEdge i
                   | otherwise                 -> mempty
                   -- the edge uv is vertical. We already established that u lies above
                   -- q, so it lies on the edge only if v lies strictly below q.

                LT | q^.xCoord <= p^.xCoord -> mempty
                   -- the predecessor vertex p and v lie on the same side of the vertical line
                   -- through u. So we don't count this vertex/edge
                   -- TODO: not sure if this should be <= or <
                   | otherwise              -> belowLineSeg i v u
                GT | q^.xCoord >= p^.xCoord -> mempty -- same as before v and p on the same side.
                   | otherwise              -> belowLineSeg i u v


    -- | count the edge if q is below the line through l and r,
    --
    -- pre: l is left of r.
    -- pre: q-x lies in the interval [lx,rx]
    belowLineSeg i l r = case ccw l q r of
      CW       -> mempty -- q lies strictly above the segment lr
      CoLinear -> OnEdge i
      CCW      -> NumStrictlyAbove 1  -- q lies strictly below the segment lr
