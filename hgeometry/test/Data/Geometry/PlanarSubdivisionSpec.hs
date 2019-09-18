{-# LANGUAGE PartialTypeSignatures #-}
module Data.Geometry.PlanarSubdivisionSpec where


import qualified Algorithms.Geometry.PolygonTriangulation.MakeMonotone as MM
import qualified Algorithms.Geometry.PolygonTriangulation.Triangulate as TR
import qualified Algorithms.Geometry.PolygonTriangulation.TriangulateMonotone as TM
import           Control.Lens hiding (holesOf)
import           Data.Bifunctor (second)
import           Data.Either (lefts)
import           Data.Ext
import           Data.Foldable (toList, forM_)
import           Data.Geometry.Interval
import           Data.Geometry.Ipe
import           Data.Geometry.Point
import           Data.Geometry.LineSegment hiding (endPoints)
import           Data.Geometry.PlanarSubdivision
import qualified Data.Geometry.PlanarSubdivision as PS
import           Data.Geometry.PlanarSubdivision.Draw
import           Data.Geometry.Polygon
import qualified Data.List as L
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Maybe (fromJust)
import           Data.PlanarGraph (FaceId(..),VertexId(..))
import qualified Data.PlaneGraph as PG
import           Data.PlaneGraph.Draw
import           Data.Range
import           Data.Ratio
import qualified Data.Vector as V
import           Test.Hspec

--------------------------------------------------------------------------------

data Test = Test
data Id a = Id a



simplePg  :: PlanarSubdivision Test () () PolygonFaceData Rational
simplePg  = fromSimplePolygon (Id Test) simplePg' Inside Outside

simplePg' :: SimplePolygon () Rational
simplePg' = toCounterClockWiseOrder . fromPoints $ map ext $ [ Point2 160 736
                                                             , Point2 128 688
                                                             , Point2 176 672
                                                             , Point2 256 672
                                                             , Point2 272 608
                                                             , Point2 384 656
                                                             , Point2 336 768
                                                             , Point2 272 720
                                                             ]

triangle :: PlanarSubdivision Test () () PolygonFaceData Rational
triangle = (\pg -> fromSimplePolygon (Id Test) pg Inside Outside)
         $ trianglePG

trianglePG = fromPoints . map ext $ [origin, Point2 10 0, Point2 10 10]


toNonEmpty :: Foldable f => f a -> NonEmpty.NonEmpty a
toNonEmpty = NonEmpty.fromList . toList

spec :: Spec
spec = do
  describe "PlanarSubdivision" $ do
    it "outerFaceId = 0 " $
      outerFaceId triangle `shouldBe` (FaceId $ VertexId 0)
    it "outerFace tests" $
      let [d] = toList $ holesOf (outerFaceId triangle) triangle
      in leftFace d triangle `shouldBe` (outerFaceId triangle)
    noEmptyFacesSpec
    testSpec testPoly
    testSpec testPoly2
    testSpec testPoly3
    testSpec testPoly4
    testSpec simplePg'

    -- describe "incidentDarts" $ do
    --   forM_ (darts' triangle) $ \d ->
    --     it "incidentDarts indiv"  $
    --       boundary' d triangle `shouldBe` (toNonEmpty $ edges' triangle)
    -- this last test is nonsense


sameAsConnectedPG      :: (Eq v, Eq e, Eq f, Eq r, Show v, Show e, Show f, Show r)
                       => PlaneGraph s v e f r -> PlanarSubdivision s v e f r
                       -> Spec
sameAsConnectedPG g ps = describe "connected planarsubdiv, same as PlaneGraph" $ do
  it "same number of vertices" $
    PG.numVertices g `shouldBe` PS.numVertices ps
  it "same number of darts" $
    PG.numDarts g `shouldBe` PS.numDarts ps
  it "same number of edges" $
    PG.numEdges g `shouldBe` PS.numEdges ps
  it "same number of faces" $
    PG.numFaces g `shouldBe` PS.numFaces ps
  it "same vertices" $
    PG.vertices g `shouldBe` vertices ps
  it "same dart data" $
    (g^.PG.rawDartData) `shouldBe` ((^.dataVal) <$> ps^.rawDartData)
  -- it "same dart endpoints" $ do
  describe "same darts" $ do
    forM_ (darts' ps) $ \d ->
      it ("sameDarts: " ++ (show d)) $ endPoints d ps `shouldBe` PG.endPoints d g
  -- sameDarts g ps
  it "same edges" $
    (V.fromList . L.sortOn fst . toList $ PG.edgeSegments g) `shouldBe` edgeSegments ps
  it "same edges per vertex" $
    forM_ (PG.vertices' g) $ \v ->
      PG.incidentEdges v g `shouldBe` PS.incidentEdges v ps
  -- it "same face Id's" $
  --   PG.faces' g `shouldBe` faces' ps
  -- it "same outerface boundary" $
  --   (second (FaceData mempty)
  -- it "same faces" $
  --   (second (FaceData mempty) <$> PG.faces g) `shouldBe` faces ps


-- sameDart g ps d =

-- sameDarts          :: (Eq v, Eq e, Eq f, Eq r, Show v, Show e, Show f, Show r)
--                        => PlaneGraph s v e f r -> PlanarSubdivision s v e f r
--                        -> Spec
-- sameDarts g ps =
--     -- sameDart g ps


-- sort' = V.fromList . L.sortOn fst . toList




testSpec    :: (Ord r, Eq p, Fractional r, Show r, Show p)
            => SimplePolygon p r -> Spec
testSpec pg = do
  let ps = PS.fromSimplePolygon (Id Test) pg Inside Outside
  sameAsConnectedPG (PG.fromSimplePolygon (Id Test) pg Inside Outside) ps
  -- sameAsConnectedPG (TM.triangulate' (Id Test) pg)
  --                   (TM.triangulate  (Id Test) pg)
  sameAsConnectedPG (TR.triangulate' (Id Test) pg) (TR.triangulate  (Id Test) pg)
  it "correct outerface" $ do
    let i = outerFaceId ps
        x = ps^.dataOf i
    x `shouldBe` Outside


testPoly :: SimplePolygon () Rational
testPoly = toCounterClockWiseOrder . fromPoints $ map ext $ [
                                                              Point2 128 720
                                                            , Point2 192 752
                                                            , Point2 224 720
                                                            , Point2 240 672
                                                            , Point2 128 624
                                                            , Point2 176 672
                                                            ]


testPoly2 :: SimplePolygon () Rational
testPoly2 = toCounterClockWiseOrder . fromPoints $ map ext $ [ Point2 160 736
                                                             , Point2 128 688
                                                             , Point2 176 672
                                                             , Point2 256 672
                                                             , Point2 272 608
                                                             , Point2 384 656
                                                             , Point2 336 768
                                                             , Point2 272 720
                                                             ]



testPoly3 :: SimplePolygon () Rational
testPoly3 = toCounterClockWiseOrder . fromPoints $ map ext $ [ Point2 352 367
                                                             , Point2 128 176
                                                             , Point2 240 336
                                                             , Point2 80 272
                                                             , Point2 48 400
                                                             , Point2 96 384
                                                             , Point2 240 496
                                                             ]



testPoly4 :: SimplePolygon () Rational
testPoly4 = toCounterClockWiseOrder . fromPoints $ map ext $ [ Point2 64 544
                                                             , Point2 320 527
                                                             , Point2 208 496
                                                             , Point2 48 432
                                                             , Point2 16 560
                                                             ]

testPoly5 :: SimplePolygon () Rational
testPoly5 = toCounterClockWiseOrder . fromPoints $ map ext $ [ Point2 352 384
                                                             , Point2 128 176
                                                             , Point2 224 320
                                                             , Point2 48 400
                                                             , Point2 160 384
                                                             , Point2 240 496
                                                             ]


testPolyP  = fromSimplePolygon (Id Test) testPoly5 Inside Outside
testPolygPlaneG = fromJust $ testPolyP^?components.ix 0

monotonePs = MM.makeMonotone (Id Test) testPoly5
monotonePlaneG = fromJust $ monotonePs^?components.ix 0

test = TR.triangulate (Id Test) testPoly5
test' = TR.triangulate' (Id Test) testPoly5
-- test = asIpe drawPlaneGraph testPolygPlaneG mempty

-- printMP = mapM_ printAsIpeSelection
--         . map (iO' . (^.core) . snd)
--         . toList . rawFacePolygons $ monotonePs



-- printP = mapM_ printAsIpeSelection
--        . map (iO' . (^.core) . snd)
--        . toList . PG.rawFacePolygons $ test'


-- printPPX = mapM_ printAsIpeSelection
--         . map (iO' . (^.core) . snd)
--         . toList . rawFacePolygons

-- printPP = printPPX test

-- parts' = map (\pg -> fromSimplePolygon (Id Test) pg Inside Outside)
--        . lefts . map ((^.core) . snd) . toList . rawFacePolygons $ monotonePs

-- parts'' = lefts . map ((^.core) . snd) . toList . rawFacePolygons $ monotonePs


--------------------------------------------------------------------------------

noEmptyFacesSpec :: Spec
noEmptyFacesSpec = describe "fromConnectedSegments, correct handling of high degree vertex" $ do
    it "ps1" $
      draw' testSegs `shouldBe` mempty
    it "ps5" $
      draw' testSegs2 `shouldBe` mempty
    it "ps3" $
      draw' testSegs3 `shouldBe` mempty
    -- segs4 <- runIO $ readFromIpeFile "test/Data/Geometry/connectedsegments_simple2.ipe"
    -- it "connected_simple2.ipe" $
    --   draw' segs4 `shouldBe` mempty
    -- segs2 <- runIO $ readFromIpeFile "test/Data/Geometry/connectedsegments_simple.ipe"
    -- it "connected_simple.ipe" $
    --   draw' segs2 `shouldBe` mempty
    -- segs3 <- runIO $ readFromIpeFile "test/Data/Geometry/connectedsegments.ipe"
    -- it "connectedsegments.ipe" $
    --   draw' segs3 `shouldBe` mempty
  where
    draw' = draw . fromConnectedSegments (Identity Test1)

readFromIpeFile    :: FilePath -> IO [LineSegment 2 () Rational :+ _]
readFromIpeFile fp = do Right page <- readSinglePageFile fp
                        pure $
                           page^..content.traverse._withAttrs _IpePath _asLineSegment

data Test1 = Test1

testX = do segs <- readFromIpeFile "test/Data/Geometry/connectedsegments_simple2.ipe"
           let ps = fromConnectedSegments (Identity Test1) segs
           print $ draw ps


draw = V.filter isEmpty . rawFacePolygons
  where
    isEmpty (_,Left  p :+ _) = (< 3) . length . polygonVertices $ p
    isEmpty (_,Right p :+ _) = (< 3) . length . polygonVertices $ p

testSegs = map (\(p,q) -> ClosedLineSegment (ext p) (ext q) :+ ())
                   [ (origin, Point2 10 10)
                   , (origin, Point2 12 10)
                   , (origin, Point2 20 5)
                   , (origin, Point2 13 20)
                   , (Point2 10 10, Point2 12 10)
                   , (Point2 10 10, Point2 13 20)
                   , (Point2 12 10, Point2 20 5)
                   ]

testSegs2 = map (\(p,q) -> ClosedLineSegment (ext p) (ext q) :+ ())
                   [ (Point2 160 192, Point2 80 112)
                   , (Point2 80 112, Point2 192 96)
                   , (Point2 192 96, Point2 160 192)
                   ]


testSegs3 = map (\(p,q) -> ClosedLineSegment (ext p) (ext q) :+ ())
                   [ (origin, Point2 10 0)
                   , (Point2 10 0, Point2 10 10)
                   , (origin, Point2 10 10)

                   , (origin, Point2 (-10) 0)
                   , (Point2 (-10) 0, Point2 (-10) (-10))
                   , (origin, Point2 (-10) (-10))
                   ]
