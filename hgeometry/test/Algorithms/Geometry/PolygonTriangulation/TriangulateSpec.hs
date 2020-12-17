{-# LANGUAGE OverloadedStrings #-}
module Algorithms.Geometry.PolygonTriangulation.TriangulateSpec (spec) where

import           Algorithms.Geometry.PolygonTriangulation.Triangulate
import           Algorithms.Geometry.PolygonTriangulation.Types
import           Control.Lens
import           Data.Ext
import           Data.Geometry
import           Data.Geometry.PlanarSubdivision                      (PolygonFaceData)
import           Data.Geometry.PolygonSpec ()
import           Data.PlaneGraph
import           Data.Proxy
import qualified Data.Vector                                          as V
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Instances                   ()


spec :: Spec
spec = do
  it "sum (map area (triangulate polygon)) == area polygon" $ do
    property $ \(poly :: SimplePolygon () Rational) ->
      let g = triangulate' Proxy poly
          trigs = graphPolygons g
      in sum (map area trigs) === area poly
  it "all isTriangle . triangulate" $ do
    property $ \(poly :: SimplePolygon () Rational) ->
      let g = triangulate' Proxy poly
          trigs = graphPolygons g
      in all isTriangle trigs

graphPolygons :: (Ord r, Fractional r) => PlaneGraph s p PolygonEdgeType PolygonFaceData r -> [SimplePolygon p r]
graphPolygons g =
  map (view core) $
  map (`rawFacePolygon` g) $
  map fst $ V.toList (internalFaces g)
