{-# LANGUAGE UndecidableInstances #-}
module PlaneGraph.FromPolygonsSpec where

import Control.Lens
import Data.Coerce
import Data.Foldable1
import GHC.Generics (Generic)
import HGeometry.PlaneGraph
import HGeometry.Polygon
import Hiraffe.PlanarGraph
import Test.Hspec

--------------------------------------------------------------------------------


--------------------------------------------------------------------------------



{-
fromPolygons :: ( Polygon_ polygon vertex r
                , Foldable1 nonEmpty
                ) => nonEmpty polygon -> PlaneGraph w s vertex () [polygon]
fromPolygons = undefined
-}



-- instance ( -- PlanarGraph_ (Component s)
--          -- , IsComponent s
--          -- , EdgeIx   (Component s) ~ Dart.Dart (Wrap s)
--          -- , Edge     (Component s) ~ Dart.Dart s
--          ) => PlanarGraph_ (PlanarGraph w s v e f) where
--   -- dualGraph, (incidentFaceOf | leftFaceOf), rightFaceOf, prevDartOf, nextDartOf, boundaryDartOf, boundaryDartOf, boundaryDarts
--   type DualGraphOf (PlanarGraph w s v e f) = CPlanarGraph (DualOf w) s f e v
--   type WorldOf     (PlanarGraph w s v e f) = w

--   dualGraph g =  undefined
--   _DualFaceIx     _ = undefined
--   _DualVertexIx   _ = undefined
--   incidentFaceOf  d = undefined
--   rightFaceOf     d = undefined
--   prevDartOf      d = undefined
--   nextDartOf      d = undefined
--   boundaryDartOf  f = undefined
--   boundaryDarts   f g = undefined


spec :: Spec
spec = describe "Constructing a PlaneGraph from overlapping Polygons" $ do
         pure ()
