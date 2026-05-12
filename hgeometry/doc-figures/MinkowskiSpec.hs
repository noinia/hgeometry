{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-incomplete-uni-patterns #-}
module MinkowskiSpec(spec) where

import R
import HGeometry.Polygon.Convex.MinkowskiSum
import HGeometry.Miso.Svg.StaticCanvas
import HGeometry.Miso.Svg
import HGeometry.Miso.OrphanInstances ()
import System.OsPath
import Miso.Svg.Property
import HGeometry
import HGeometry.Polygon
import Miso.String (ToMisoString(..))
import Golden
import Test.Hspec
import Test.Hspec.WithTempFile

--------------------------------------------------------------------------------

spec :: Spec
spec = goldenWith [osp|data/doc-figures/|]
         (svgFileGolden { name = [osp|minkowskiSum|] })
           (staticCanvas_ canvas []
               [ draw (minkowskiSum basePolygon offsetPolygon) [fill_ "red", stroke_ "black"]
               , draw basePolygon [fill_ "blue", stroke_ "black"]
               , draw (translateBy (Vector2 40 40) offsetPolygon
                      ) [fill_ "green", stroke_ "black"]
               ]
           )

basePolygon :: ConvexPolygon (Point 2 R)
Just basePolygon = fromPoints [ Point2 80 112
                              , Point2 96 144
                              , Point2 160 176
                              , Point2 208 128
                              , Point2 192 48
                              , Point2 128 16
                              ]

offsetPolygon :: ConvexPolygon (Point 2 R)
Just offsetPolygon = fromPoints [ Point2 0     10
                                , Point2 (-10) (-10)
                                , Point2 10    (-10)
                                ]

canvas :: StaticCanvas R
canvas = staticCanvas 240 200
