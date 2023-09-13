{-# LANGUAGE OverloadedStrings #-}
module HGeometry.Polygon.Triangulation.TriangulateSpec (spec) where

import           Control.Lens
import qualified Data.Vector as V
import           HGeometry
import           HGeometry.Ext
import           HGeometry.PlanarSubdivision (PolygonFaceData)
import           HGeometry.PlaneGraph
import           HGeometry.PolygonSpec ()
import           HGeometry.PolygonTriangulation.Triangulate
import           HGeometry.PolygonTriangulation.Types
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

spec :: Spec
spec = do
  it "sum (map area (triangulate polygon)) == area polygon" $ do
    property $ \(poly :: SimplePolygon () Rational) ->
      let g = triangulate' @() poly
          trigs = graphPolygons g
      in sum (map area trigs) === area poly
  it "all isTriangle . triangulate" $ do
    property $ \(poly :: SimplePolygon () Rational) ->
      let g = triangulate' @() poly
          trigs = graphPolygons g
      in all isTriangle trigs

graphPolygons   :: (Ord r, Fractional r)
                => PlaneGraph s p PolygonEdgeType PolygonFaceData r -> [SimplePolygon p r]
graphPolygons g = map (^._2.core) . V.toList . snd $ facePolygons (outerFaceId g) g
