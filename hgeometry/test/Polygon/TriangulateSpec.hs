{-# LANGUAGE OverloadedStrings #-}
module Polygon.TriangulateSpec (spec) where

import           Control.Lens
import           HGeometry
import           HGeometry.Ext
import           HGeometry.Number.Real.Rational
import           HGeometry.PlaneGraph
import           HGeometry.Polygon
import           HGeometry.Polygon.Instances ()
import           HGeometry.Polygon.Simple
import           HGeometry.Polygon.Triangulation
import qualified HGeometry.Polygon.Triangulation.TriangulateMonotone as TM
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()



-- import           Debug.Trace
--------------------------------------------------------------------------------

type R = RealNumber 5



spec :: Spec
spec = do
  it "buggy polygon diagionals" $
    computeDiagonals buggyPolygon `shouldBe` [ Vector2 3 1
                                             , Vector2 3 0
                                             ]
  it "buggy polygon monotone area" $
    let g     = TM.triangulate @() buggyPolygon
        trigs = graphPolygons g
    in sum (map area trigs) `shouldBe` area buggyPolygon

  it "buggy polygon area" $
    let g     = triangulate @() buggyPolygon
        trigs = graphPolygons g
    in sum (map area trigs) `shouldBe` area buggyPolygon
  prop "sum (map area (triangulate polygon)) == area polygon" $
    \(poly :: SimplePolygon (Point 2 R)) ->
      let g = triangulate @() poly
          trigs = graphPolygons g
      in counterexample (show g) $ sum (map area trigs) === area poly
  prop "all isTriangle . triangulate" $
    \(poly :: SimplePolygon (Point 2 R)) ->
      let g = triangulate @() poly
          trigs = graphPolygons g
      in all isTriangle trigs

  -- prop "creating the graph does not create additional diagionals" $
  --   \poly ->
  --     let gr     = triangulate @() poly
  --         algSol = extractDiagonals gr
  --     in naiveSet algoSol `shouldBe` naiveSet (computeDiagionals poly)



buggyPolygon :: SimplePolygon (Point 2 R)
buggyPolygon = read "SimplePolygon [Point2 9 13,Point2 3 6,Point2 0 3,Point2 2 3,Point2 0 0]"

isTriangle :: SimplePolygon (Point 2 R) -> Bool
isTriangle = (== 3) . numVertices

graphPolygons    :: (Ord r, Num r, Point_ point 2 r)
                 => PlaneGraph s point PolygonEdgeType PolygonFaceData
                 -> [SimplePolygon (Point 2 r)]
graphPolygons gr = map (&vertices %~ view (core.asPoint)) $ gr^..interiorFacePolygons
