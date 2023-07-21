{-# LANGUAGE OverloadedStrings #-}
module Polygon.TriangulateSpec (spec) where

import           Control.Lens
import qualified Data.Vector as V
import           HGeometry
import           HGeometry.Ext
-- import           HGeometry.PlanarSubdivision (PolygonFaceData)
-- import           HGeometry.PlaneGraph
-- import           HGeometry.PolygonSpec ()
-- import           HGeometry.PolygonTriangulation.Triangulate
-- import           HGeometry.PolygonTriangulation.Types
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck.Instances ()

--------------------------------------------------------------------------------

spec :: Spec
spec = do
  pure ()

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
