{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
module ClosestPair (closestPairShowcase) where

import Algorithms.Geometry.ClosestPair.DivideAndConquer (closestPair)

import           Control.Lens                 ((&))
import           Control.Monad.Random         (evalRand, forM_, mkStdGen, replicateM)
import           Data.Ext                     (ext, type (:+) ((:+)))
import           Geometry.Point          (Point (Point2, toVec), euclideanDist)
import           Geometry.Transformation (scaleUniformlyBy)
import           Geometry.Vector         (Affine ((.+^), (.-^)), (^/))
import           Data.Hashable                (Hashable (hash))
import qualified Data.LSeq                    as LSeq
import           Data.RealNumber.Rational
import qualified Data.Vector.Circular         as CV
import           Linear.V2                    (V2 (V2), unangle)
import           Reanimate                    (Animation, SVG, animate, applyE, curveS,
                                               defaultStrokeWidth, destroySprite, fadeOutE,
                                               mkCircle, mkEllipse, mkGroup, newSpriteA',
                                               overEnding, partialSvg, pathify, pauseAtEnd, play,
                                               rotate, scene, setDuration, signalA, translate,
                                               withFillColorPixel, withFillOpacity,
                                               withStrokeColorPixel, withStrokeWidth)
import           Reanimate.Animation          (Sync (SyncFreeze))

import Common (black, genPoints, green, lerpPoint, nodeRadius)

type R = RealNumber 10

-- closestPair :: (Ord r, Num r) => LSeq 2 (Point 2 r :+ p) -> Two (Point 2 r :+ p)

-- mapA (withViewBox (screenBottom, screenBottom, screenHeight, screenHeight)) $
closestPairShowcase :: Animation
closestPairShowcase = scene $ do
  forM_ [0 .. length points-1] $ \i -> do
    let this = CV.index points i
        next = CV.index points (i+1)
    s <- newSpriteA' SyncFreeze $ animateTransition (zip this next)
      & signalA (curveS 2)
      & setDuration 2
    play $ showPair next
      & signalA (curveS 2)
      & setDuration 2
      & pauseAtEnd 1
      & applyE (overEnding 0.2 fadeOutE)
    destroySprite s

animateTransition :: [(Point 2 R, Point 2 R)] -> Animation
animateTransition pairs = animate $ \t ->
  showPoints [ lerpPoint t b a | (a,b) <- pairs ]

showPair :: [Point 2 R] -> Animation
showPair pts = animate $ \t ->
    withStrokeColorPixel black $
    withFillOpacity 0 $
    partialSvg t $
    pathify $
    translate centerX centerY $
    rotate ang $
    mkEllipse (dist/2 + nodeRadius*2) (nodeRadius*2)
  where
    V2 (a :+ _) (b :+ _) = closestPair (LSeq.promise $ LSeq.fromList $ map ext pts)
    p1 = realToFrac <$> a
    p2 = realToFrac <$> b
    Point2 centerX centerY = p1 .+^ toVec (p2 .-^ toVec p1) ^/ 2
    dist = euclideanDist p1 p2
    ang = unanglePoint p1 p2 / pi * 180

unanglePoint :: (Floating r, Ord r) => Point 2 r -> Point 2 r -> r
unanglePoint a b = unangle (V2 x y)
  where
    Point2 x y = a .-^ toVec b

showPoints :: [Point 2 R] -> SVG
showPoints = mkGroup . map ppPoint

seed :: Int
seed = hash "closestpair"

nSets :: Int
nSets = 10

nPoints :: Int
nPoints = 10

points :: CV.CircularVector [Point 2 R]
points = CV.unsafeFromList $ map (map $ scaleUniformlyBy 0.9) $ flip evalRand (mkStdGen seed) $
  replicateM nSets (genPoints nPoints)

ppPoint :: Real r => Point 2 r -> SVG
ppPoint (Point2 x y) =
  withFillColorPixel green $
  withStrokeColorPixel black $
  translate (realToFrac x) (realToFrac y) $
  mkCircle nodeRadius

