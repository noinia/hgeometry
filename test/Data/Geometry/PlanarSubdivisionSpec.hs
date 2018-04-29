module Data.Geometry.PlanarSubdivisionSpec where


import qualified Algorithms.Geometry.PolygonTriangulation.MakeMonotone as MM
import           Data.Bifunctor (second)
import           Data.Ext
import           Data.Foldable (toList, forM_)
import           Data.Geometry
import           Data.Geometry.PlanarSubdivision
import qualified Data.Geometry.PlanarSubdivision as PS
import           Data.Geometry.Polygon
import qualified Data.List.NonEmpty as NonEmpty
import           Data.PlanarGraph (FaceId(..),VertexId(..))
import qualified Data.PlaneGraph as PG
import           Test.Hspec
import qualified Data.Vector as V
import qualified Data.List as L

import qualified Algorithms.Geometry.PolygonTriangulation.TriangulateMonotone as TM
import qualified Algorithms.Geometry.PolygonTriangulation.Triangulate as TR

import           Data.Maybe (fromJust)
import           Control.Lens hiding (holesOf)
import           Data.PlaneGraph.Draw
import           Data.Geometry.Ipe
import           Data.Geometry.PlanarSubdivision.Draw


data Test = Test
data Id a = Id a




simplePg  = fromSimplePolygon (Id Test) simplePg' Inside Outside
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
    testSpec testPoly
    testSpec testPoly2
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
  sameAsConnectedPG (PG.fromSimplePolygon (Id Test) pg Inside Outside)
                    (PS.fromSimplePolygon (Id Test) pg Inside Outside)
  -- sameAsConnectedPG (TM.triangulate' (Id Test) pg)
  --                   (TM.triangulate  (Id Test) pg)
  sameAsConnectedPG (TR.triangulate' (Id Test) pg)
                    (TR.triangulate  (Id Test) pg)



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


testPolyP  = fromSimplePolygon (Id Test) testPoly2 Inside Outside
testPolygPlaneG = fromJust $ testPolyP^?components.ix 0

monotonePs = MM.makeMonotone (Id Test) testPoly2
monotonePlaneG = fromJust $ monotonePs^?components.ix 0

test = TR.triangulate (Id Test) testPoly2
test' = TR.triangulate' (Id Test) testPoly2
-- test = asIpe drawPlaneGraph testPolygPlaneG mempty

printMP = mapM_ printAsIpeSelection
        . map (asIpeObject' mempty . (^.core) . snd)
        . toList . rawFacePolygons $ monotonePs



printP = mapM_ printAsIpeSelection
       . map (asIpeObject' mempty . (^.core) . snd)
       . toList . PG.rawFacePolygons $ test'


printPP = mapM_ printAsIpeSelection
        . map (asIpeObject' mempty . (^.core) . snd)
        . toList . rawFacePolygons $ test
