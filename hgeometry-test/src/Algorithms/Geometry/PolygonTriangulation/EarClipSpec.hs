{-# LANGUAGE OverloadedStrings #-}
module Algorithms.Geometry.PolygonTriangulation.EarClipSpec (spec) where

import Algorithms.Geometry.PolygonTriangulation.EarClip

import           Control.Lens
import           Data.Ext
import           Data.Geometry
import           Data.Geometry.PlanarSubdivision (PolygonFaceData)
import           Data.Geometry.PolygonSpec       ()
import qualified Data.Geometry.Triangle          as Triangle
import           Data.PlaneGraph
import           Data.Proxy
import qualified Data.Vector                     as V
import qualified Data.Vector.Circular            as CV
import qualified Data.Vector.NonEmpty            as NE
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Instances       ()

spec :: Spec
spec = do
  it "sum (map area (triangulate polygon)) == area polygon" $ do
    property $ \(poly :: SimplePolygon () Rational) ->
      let g = earClip poly
          trigs = map (polygonTrig poly) g
      in sum (map Triangle.area trigs) === area poly

polygonTrig :: SimplePolygon p r -> (Int,Int,Int) -> Triangle.Triangle 2 p r
polygonTrig p (a,b,c) = Triangle.Triangle (vs V.! a) (vs V.! b) (vs V.! c)
  where
    vs = NE.toVector $ CV.vector $ p^.outerBoundaryVector
