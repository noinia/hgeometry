{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes #-}
module Polygon.Triangulation.TriangulateSpec (spec) where

import           Control.Lens
import           Debug.Trace
import           HGeometry
import           HGeometry.Ext
import           HGeometry.Number.Real.Rational
import           HGeometry.PlaneGraph
import           HGeometry.Polygon.Class
import           HGeometry.Polygon.Simple
import           HGeometry.Polygon.Triangulation
import qualified HGeometry.Polygon.Triangulation.TriangulateMonotone as TM
import           HGeometry.Transformation
import           Ipe
import           PlaneGraph.RenderSpec (drawVertex, drawDart, drawFace, drawEdge)
import           System.OsPath
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

--------------------------------------------------------------------------------

type R = RealNumber 5

spec :: Spec
spec = describe "triangulateSpec" $ do
  let g     = traceShowWith (\g' -> ("g", g',"dual g",dualGraph g')) $ TM.triangulate @() buggyPolygon
      faces' = g^..interiorFacePolygons
      trigs = graphPolygons $ traceShowWith ("faces",faces',) $ g
  runIO $ writeIpeFile [osp|/tmp/out.ipe|] . singlePageFromContent $ drawGraph $ scaleUniformlyBy 10 g
  -- runIO $ writeIpeFile [osp|/tmp/dual.ipe|] . singlePageFromContent $ drawGraph $ scaleUniformlyBy 10 (dualGraph g)
  it "buggy polygon monotone area" $ do
    sum (map area trigs) `shouldBe` area buggyPolygon


--------------------------------------------------------------------------------

drawGraph    :: ( PlaneGraph_ planeGraph vertex
                , IsTransformable vertex
                , Point_ vertex 2 r, Ord r, Real r, Fractional r, Show r, Eq (FaceIx planeGraph)
                , Show (Vertex planeGraph), Show (Dart planeGraph), Show (Face planeGraph)
                , Show (EdgeIx planeGraph)
                ) => planeGraph -> [IpeObject r]
drawGraph gr = theVertices <> theEdges <> theFaces
  where
    theVertices = ifoldMapOf vertices             drawVertex    gr
    theEdges    = ifoldMapOf dartSegments         (drawDart gr) gr
               <> ifoldMapOf edgeSegments         (drawEdge gr) gr
    theFaces    = [] -- ifoldMapOf interiorFacePolygons (drawFace gr) gr


buggyPolygon :: SimplePolygon (Point 2 R)
buggyPolygon = read "SimplePolygon [Point2 9 9,Point2 3 6,Point2 0 3,Point2 2 3,Point2 0 0]"

graphPolygons    :: (Ord r, Num r, Point_ point 2 r)
                 => PlaneGraph s point PolygonEdgeType PolygonFaceData
                 -> [SimplePolygon (Point 2 r)]
graphPolygons gr = map (&vertices %~ view (core.asPoint)) $ gr^..interiorFacePolygons


-- spec :: Spec
-- spec = do testCases [osp|test-with-ipe/Polygon/Triangulation/monotone.ipe|]
--           -- testCases [osp|test-with-ipe/Polygon/Triangulation/simplepolygon6.ipe|]

-- testCases    :: OsPath -> Spec
-- testCases fp = (runIO $ readInput =<< getDataFileName fp) >>= \case
--     Left e    -> it "reading TriangulateMonotone file" $
--                    expectationFailure $ "Failed to read ipe file " ++ show e
--     Right tcs -> mapM_ toSpec tcs

-- data TestCase r = TestCase { _polygon  :: SimplePolygon (Point 2 r) :+ IpeColor r
--                            , _solution :: [ClosedLineSegment (Point 2 r)]
--                            }
--                   deriving (Show,Eq)

-- toSpec                            :: (Num r, Ord r, Show r) => TestCase r -> Spec
-- toSpec (TestCase (poly :+ c) sol) =
--     describe ("testing polygions of color " ++ show c) $ do
--       it "comparing with manual solution" $ do
--         let algSol = triangulate @() poly
--         (naiveSet . map toSeg $ algSol) `shouldBe` naiveSet sol
