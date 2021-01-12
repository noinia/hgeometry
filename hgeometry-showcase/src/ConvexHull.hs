{-# LANGUAGE DataKinds #-}
module ConvexHull (convexHullShowcase) where

import Algorithms.Geometry.ConvexHull.GrahamScan

import           Control.Lens                 ((&), (^.))
import           Control.Monad.Random         (evalRand, forM_, mkStdGen, replicateM)
import           Data.Ext                     (ext)
import           Data.Geometry.Point          (Point (Point2))
import           Data.Geometry.Polygon.Convex (simplePolygon)
import           Data.Geometry.Transformation (scaleUniformlyBy)
import qualified Data.List.NonEmpty           as NonEmpty
import qualified Data.Vector.Circular         as CV
import           Reanimate                    (Animation, SVG, animate, curveS, mkCircle, mkGroup,
                                               pauseAtEnd, play, scene, setDuration, signalA,
                                               translate, withFillColorPixel, withStrokeColorPixel)

import Common (black, genPoints, green, grey, lerpPoint, nodeRadius, ppPolygonBody,
               ppPolygonOutline)

-- mapA (withViewBox (screenBottom, screenBottom, screenHeight, screenHeight)) $
convexHullShowcase :: Animation
convexHullShowcase = scene $ do
  forM_ [0 .. length points-1] $ \i -> do
    let this = CV.index points i
        next = CV.index points (i+1)
    play $ animateTransition (zip this next)
      & signalA (curveS 2)
      & setDuration 2
      & pauseAtEnd 1

animateTransition :: [(Point 2 Rational, Point 2 Rational)] -> Animation
animateTransition pairs = animate $ \t ->
  let pts = [ lerpPoint t b a | (a,b) <- pairs ]
  in mkGroup
    [ showHull pts
    , showPoints pts ]

showHull :: [Point 2 Rational] -> SVG
showHull pts = mkGroup
    [ ppPolygonBody grey (poly^.simplePolygon)
    , ppPolygonOutline black (poly^.simplePolygon) ]
  where
    poly = convexHull (NonEmpty.fromList $ map ext pts)

showPoints :: [Point 2 Rational] -> SVG
showPoints pts = mkGroup
  [ ppPoint pt
  | pt <- pts
  ]

seed :: Int
seed = 0xDEADBEEF

nSets :: Int
nSets = 10

nPoints :: Int
nPoints = 10

points :: CV.CircularVector [Point 2 Rational]
points = CV.unsafeFromList $ map (map $ scaleUniformlyBy 0.9) $ flip evalRand (mkStdGen seed) $
  replicateM nSets (genPoints nPoints)

ppPoint :: Real r => Point 2 r -> SVG
ppPoint (Point2 x y) =
  withFillColorPixel green $
  withStrokeColorPixel black $
  translate (realToFrac x) (realToFrac y) $
  mkCircle nodeRadius

