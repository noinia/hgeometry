{-# LANGUAGE DataKinds #-}
module UniformSampling (uniformSamplingShowcase) where

import           Algorithms.Geometry.ConvexHull.GrahamScan
import           Algorithms.Geometry.UniformPolygonSampling
import           Common
import           Control.Lens                               ((&), (^.))
import           Control.Monad.Random                       (evalRand, forM_, mkStdGen, replicateM)
import           Data.Ext                                   (ext)
import           Data.Geometry.Point                        (Point (Point2))
import           Data.Geometry.Polygon
import           Data.Geometry.Polygon.Convex
import           Data.Geometry.Transformation               (scaleUniformlyBy)
import qualified Data.List.NonEmpty                         as NonEmpty
import           Data.RealNumber.Rational
import qualified Data.Vector.Circular                       as CV
import           Reanimate

type R = RealNumber 10

targetPolygon :: SimplePolygon () Double
targetPolygon = scaleUniformlyBy 2 $ pAtCenter $ simpleFromPoints $ map ext
  [ Point2 0 0
  , Point2 1 0
  , Point2 1 1, Point2 2 1, Point2 2 (-1)
  , Point2 0 (-1), Point2 0 (-2)
  , Point2 3 (-2), Point2 3 2, Point2 0 2 ]

uniformSamplingShowcase :: Animation
uniformSamplingShowcase = scene $ do
  newSpriteSVG_ $ ppPolygonBody grey targetPolygon
  newSpriteSVG_ $ ppPolygonOutline black targetPolygon
  wait 1
  forM_ points $ \point -> do
    fork $ newSpriteSVG_ $ ppPoint point
    wait 0.025

nPoints :: Int
nPoints = 1000

points :: [Point 2 Double]
points = flip evalRand (mkStdGen seed) $ replicateM nPoints $ samplePolygon targetPolygon

seed :: Int
seed = 0xDEADBEEF

ppPoint :: Real r => Point 2 r -> SVG
ppPoint (Point2 x y) =
  withFillColorPixel red $
  translate (realToFrac x) (realToFrac y) $
  mkCircle (nodeRadius/5)
