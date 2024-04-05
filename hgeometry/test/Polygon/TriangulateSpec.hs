{-# LANGUAGE OverloadedStrings #-}
module Polygon.TriangulateSpec (spec) where

import Control.Lens
import HGeometry
import HGeometry.Ext
import HGeometry.Number.Real.Rational
import HGeometry.PlaneGraph
import HGeometry.Polygon.Class
import HGeometry.Polygon.Instances ()
import HGeometry.Polygon.Simple
import HGeometry.Polygon.Triangulation
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Instances ()

--------------------------------------------------------------------------------

type R = RealNumber 5

spec :: Spec
spec = do
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




isTriangle = (== 3) . numVertices

graphPolygons    :: (Ord r, Num r, Point_ point 2 r)
                 => PlaneGraph s point PolygonEdgeType PolygonFaceData
                 -> [SimplePolygon (Point 2 r)]
graphPolygons gr = map (&vertices %~ view (core.asPoint)) $ gr^..interiorFacePolygons

--   prop "sum (map area (triangulate polygon)) == area polygon" $
--     \(poly :: SimplePolygon () Rational) ->
--       let g = triangulate' @() poly
--           trigs = graphPolygons g
--       in sum (map area trigs) === area poly
--   prop "all isTriangle . triangulate" $
--     \(poly :: SimplePolygon () Rational) ->
--       let g = triangulate' @() poly
--           trigs = graphPolygons g
--       in all isTriangle trigs

-- graphPolygons   :: (Ord r, Fractional r)
--                 => PlaneGraph s p PolygonEdgeType PolygonFaceData r -> [SimplePolygon p r]
-- graphPolygons g = map (^._2.core) . V.toList . snd $ facePolygons (outerFaceId g) g
