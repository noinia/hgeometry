{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module UniformSampling (uniformSamplingShowcase) where

import           Data.Geometry.Polygon.Sample
import           Common
import           Control.Monad.Random                       (evalRand, forM_, mkStdGen, replicateM)
import           Data.Geometry.Point                        (Point (Point2))
import           Data.Geometry.Polygon
import           Data.Geometry.Polygon.Bezier               (approximateSome)
import           Reanimate

eps :: Double
eps = 0.001

svgPolygons :: [SomePolygon () Double]
svgPolygons = map (approximateSome eps) $
  svgToPolygons $ lowerTransformations $ scale 2 $ center $ latex "Uniform\n\nSampling"

-- targetPolygon :: SomePolygon () Double
-- targetPolygon = scaleUniformlyBy 2 $ pAtCenter $ simpleFromPoints $ map ext
--   [ Point2 0 0
--   , Point2 1 0
--   , Point2 1 1, Point2 2 1, Point2 2 (-1)
--   , Point2 0 (-1), Point2 0 (-2)
--   , Point2 3 (-2), Point2 3 2, Point2 0 2 ]

uniformSamplingShowcase :: Animation
uniformSamplingShowcase = scene $ do
  forM_ svgPolygons $ \svgPolygon -> fork $ do 
    newSpriteSVG_ $ overAny (ppPolygonBody' grey) svgPolygon
    -- newSpriteSVG_ $ overAny (ppPolygonOutline' black) targetPolygon
    wait 1
    forM_ (overAny points svgPolygon) $ \point -> do
      fork $ play $ playThenReverseA $ mkAnimation 0.5 $ \t ->
        aroundCenter (scale t) $ ppPoint point
      wait 0.025

nPoints :: Int
nPoints = 100

points :: (Show p) => Polygon t p Double -> [Point 2 Double]
points p = flip evalRand (mkStdGen seed) $ replicateM nPoints $ samplePolygon p

seed :: Int
seed = 0xDEADBEEF

ppPoint :: Real r => Point 2 r -> SVG
ppPoint (Point2 x y) =
  withFillColorPixel red $
  translate (realToFrac x) (realToFrac y) $
  mkCircle (nodeRadius/5)
