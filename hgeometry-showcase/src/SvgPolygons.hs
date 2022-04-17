{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
module SvgPolygons (svgPolygonsShowcase) where

import Algorithms.Geometry.PolygonTriangulation.Triangulate (computeDiagonals)

import Geometry.Polygon.Bezier (approximateSome)
import Reanimate

import Common

svgPolygonsShowcase :: Animation
svgPolygonsShowcase = playThenReverseA $ pauseAround 0.5 0.5 $ scene $ do
    let svgPolygons = svgToPolygons $ lowerTransformations $ center $ scale 12 $ latex "S"
    detail <- flip simpleVar 1 $ \eps ->
      let lowered = map (approximateSome eps) svgPolygons

      in mkGroup
        [ mkGroup
          [ withStrokeColorPixel polyColor $ mkGroup
            [ ppLineSegment line
            | line <- overAny computeDiagonals p
            ]
          , withStrokeWidth (defaultStrokeWidth) $
            overAny (ppPolygonOutline' black) p
          ]
        | p <- lowered
        ]
    tweenVar detail 2 $ \val -> fromToS val 0.005 . curveS 2
