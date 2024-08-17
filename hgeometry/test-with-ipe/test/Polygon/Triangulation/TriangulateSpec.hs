{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes #-}
module Polygon.Triangulation.TriangulateSpec (spec) where

import           Control.Lens
-- import           Debug.Trace
import           HGeometry
import           HGeometry.Ext
import           HGeometry.Number.Real.Rational
import           HGeometry.PlaneGraph
import           HGeometry.Polygon
import           HGeometry.Polygon.Simple
import           HGeometry.Polygon.Triangulation
import qualified HGeometry.Polygon.Triangulation.MakeMonotone as MM
import qualified HGeometry.Polygon.Triangulation.TriangulateMonotone as TM
import           HGeometry.Transformation
import           Ipe
import           PlaneGraph.RenderSpec (drawVertex, drawDart, drawEdge)
-- import           System.OsPath
import           Test.Hspec
-- import           Test.QuickCheck
-- import           Test.QuickCheck.Instances ()

--------------------------------------------------------------------------------

type R = RealNumber 5

spec :: Spec
spec = describe "triangulateSpec" $ do
  it "buggy polygon monotone area" $ do
    let g     = TM.triangulate @() buggyPolygon
        trigs = graphPolygons g
    sum (map area trigs) `shouldBe` area buggyPolygon
  -- it "all monotone polygons"
  it "monotone diags of buggypolygon2'" $
    MM.computeDiagonals buggyPolygon2' `shouldBe` [Vector2 0 3]
  it "buggyPolygon2' diagonals" $
    computeDiagonals buggyPolygon2' `shouldBe` [Vector2 0 3,Vector2 1 3]
  -- let g     = triangulate @() buggyPolygon2'
  --     trigs = graphPolygons g
  -- runIO $ do
  --   putStrLn "==============g buggy2 ===================="
  --   print g
  --   putStrLn "==============dual g buggy 2 ===================="
  --   print $ dualGraph g
  --   putStrLn "=================================="
  --   writeIpeFile [osp|/tmp/out.ipe|] . singlePageFromContent $ drawGraph $ scaleUniformlyBy 10 g
  it "buggyPolygon2' monotone area" $ do
    let g     = triangulate @() buggyPolygon2'
        trigs = graphPolygons g
    sum (map area trigs) `shouldBe` area buggyPolygon2'
  it "buggyPolygon2 monotone area" $ do
    let g     = triangulate @() buggyPolygon2
        trigs = graphPolygons g
    sum (map area trigs) `shouldBe` area buggyPolygon2

--------------------------------------------------------------------------------

_drawGraph    :: ( PlaneGraph_ planeGraph vertex
                 , IsTransformable vertex
                 , ConstructablePoint_ vertex 2 r, Ord r, Real r, Fractional r, Show r, Eq (FaceIx planeGraph)
                 , Show (Vertex planeGraph), Show (Dart planeGraph), Show (Face planeGraph)
                 , Show (EdgeIx planeGraph)
                 ) => planeGraph -> [IpeObject r]
_drawGraph gr = theVertices <> theEdges <> theFaces
  where
    theVertices = ifoldMapOf vertices             drawVertex    gr
    theEdges    = ifoldMapOf dartSegments         (drawDart gr) gr
               <> ifoldMapOf edgeSegments         (drawEdge gr) gr
    theFaces    = [] -- ifoldMapOf interiorFacePolygons (drawFace gr) gr

graphPolygons    :: (Ord r, Num r, Point_ point 2 r)
                 => PlaneGraph s point PolygonEdgeType PolygonFaceData
                 -> [SimplePolygon (Point 2 r)]
graphPolygons gr = map (&vertices %~ view (core.asPoint)) $ gr^..interiorFacePolygons

--------------------------------------------------------------------------------
buggyPolygon :: SimplePolygon (Point 2 R)
buggyPolygon = read "SimplePolygon [Point2 9 9,Point2 3 6,Point2 0 3,Point2 2 3,Point2 0 0]"


buggyPolygon2 :: SimplePolygon (Point 2 R)
buggyPolygon2 = read "SimplePolygon [Point2 0 11,Point2 52 0,Point2 68 13,Point2 38 12,Point2 15 16]"

buggyPolygon2' :: SimplePolygon (Point 2 R)
buggyPolygon2' = read "SimplePolygon [Point2 0 11,Point2 52 0,Point2 68 15,Point2 38 12,Point2 15 16]"


--------------------------------------------------------------------------------

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
