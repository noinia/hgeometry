{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ApplicativeDo #-}
module ZHashing where

import Algorithms.Geometry.PolygonTriangulation.EarClip

import           Control.Lens                 ((&), (.~), (^.))
import           Control.Monad.Random
import           Data.Ext                     (_core, core)
import qualified Data.Foldable                as F
import           Geometry.Line           (Line (..))
import           Geometry.LineSegment    (LineSegment (LineSegment))
import           Geometry.Point          (Point (Point2), squaredEuclideanDist, PointFunctor(..))
import           Geometry.Polygon        as P
import           Geometry.Transformation (scaleUniformlyBy)
import           Geometry.Vector         (Vector, pattern Vector2, quadrance)
import           Geometry.Box
import           Data.Hashable                (Hashable (hash))
import           Data.Intersection            (IsIntersectableWith (intersect),
                                               NoIntersection (NoIntersection))
import qualified Data.List.NonEmpty           as NonEmpty
import           Data.Maybe                   (catMaybes)
import           Data.Ratio                   ((%))
import           Data.RealNumber.Rational
import qualified Data.Vector.Circular         as CV
import           Data.Vinyl                   (Rec (RNil, (:&)))
import           Data.Vinyl.CoRec             (Handler (H), match)
import           Graphics.SvgTree             (Cap (..), LineJoin (..), strokeLineCap)
import           Reanimate hiding (boundingBox)
import           Linear.V2
import Data.Coerce

import Common

type R = RealNumber 5

{- Animation overview:
* Start with N random points.
* Draw polygon outline.
* Scan through polygon, highlighting intersections.
* Fade out everything except points.
* Interpolate to new random set of points.
-}

boxDim :: Num a => a
boxDim = 2^1

zHashingShowcase :: Animation
zHashingShowcase = scene $ do
  play $ drawZCurve
    & applyE (overEnding 0.5 fadeOutE)
  wait (-0.5)
  play $ zHashingRects
    & pauseAround 1 1
    & applyE (overBeginning 0.5 fadeInE)
    & applyE (overEnding 0.5 fadeOutE)

zHashingRects :: Animation
zHashingRects = scene $ do
  newSpriteSVG $ zCurveEnv (2^4) $ mkGroup
    [ mkGroup [
      withStrokeColor "grey" $
      mkLinePath
      [ (x,y)
      | i <- [0 .. 16^2-1]
      , let V2 x y = realToFrac <$> zUnHash i
      ]]
    ]
  rectVar <- adjustZ succ $ simpleVar showBB (castRect $ head boxes)
  forM_ (zip boxes (tail boxes)) $ \(rectA, rectB) -> do
    play $ drawBBCurve rectA
      & signalA (curveS 2)
      & pauseAtEnd 0.5
      & applyE (overEnding 0.2 fadeOutE)
    tweenVar rectVar 1 $ \old t -> lerpRectangle (curveS 2 t) (castRect rectB) old
    wait 0.5
  play $ drawBBCurve (last boxes)
    & signalA (curveS 2)
    & pauseAtEnd 0.5
    & applyE (overEnding 0.2 fadeOutE)

castRect :: (Integral r, Num r') => Rectangle p r -> Rectangle p r'
castRect rect = pmap (fmap fromIntegral) rect

drawZCurve :: Animation
drawZCurve = scene $ do
    zoomV <- newVar 1
    shownV <- newVar 0
    partialV <- newVar 0
    newSprite $ do
      zoom' <- unVar zoomV
      shown' <- unVar shownV
      p <- unVar partialV
      pure $
        let zoom = 2**zoom' in
        let shown = 2^shown' in
        let nextStep = 2^(shown'+1) in
        zCurveEnv zoom $
        mkGroup
        [ renderZCurve shown
        , partialSvg p $ pathify $ mkLinePath
          [ (x,y)
          | i <- [shown^2-1 .. nextStep^2-1]
          , let V2 x y = realToFrac <$> zUnHash i
          ]
        ]
    let takeStep = do
          now <- readVar shownV
          tweenVar partialV (2^now) $ \v -> fromToS v (v+1) . curveS 2
          writeVar partialV 0
          writeVar shownV (now+1)

    takeStep
    wait 1
    tweenVar zoomV 1 $ \v -> fromToS v (v+1) . curveS 2
    takeStep
    wait 1
    tweenVar zoomV 1 $ \v -> fromToS v (v+1) . curveS 2
    takeStep
    wait 1
    tweenVar zoomV 1 $ \v -> fromToS v (v+1) . curveS 2
    takeStep
    wait 5

zCurveEnv :: Double -> SVG -> SVG
zCurveEnv zoom =
  lowerTransformations .
  scale 0.9 .
  scale (screenHeight/(zoom-1)) .
  withStrokeLineJoin JoinRound .
  (strokeLineCap .~ pure CapRound) .
  translate (-(zoom-1)/2) (-(zoom-1)/2) .
  withFillOpacity 0 . withStrokeColorPixel black

renderZCurve :: Word -> SVG
renderZCurve shown =
  mkGroup
  [ mkLinePath
    [ (x,y)
    | i <- [0 .. shown^2-1]
    , let V2 x y = realToFrac <$> zUnHash i
    ]
  ]

showBB :: Rectangle () R -> SVG
showBB rect = zCurveEnv (2^4) $ mkGroup [ withFillOpacity 1 $ mkGroup
    [ withStrokeColorPixel black $ withFillOpacity 0 $
      mkLinePathClosed
      [ (realToFrac minX, realToFrac minY)
      , (realToFrac maxX, realToFrac minY)
      , (realToFrac maxX, realToFrac maxY)
      , (realToFrac minX, realToFrac maxY) ]
    , ppPoint minP
    , ppPoint maxP ]]
  where
    shown = 2^4
    minP@(Point2 minX minY) = minPoint rect ^. core
    maxP@(Point2 maxX maxY) = maxPoint rect ^. core

drawBBCurve :: Rectangle () Word -> Animation
drawBBCurve rect = mkAnimation dur $ \t -> partialSvg t $ pathify $ mkGroup
    [ zCurveEnv (2^4) $
      mkLinePath
      [ (x,y)
      | i <- [minZ .. maxZ]
      , let V2 x y = realToFrac <$> zUnHash i
      ]
    ]
  where
    dur = sqrt (realToFrac (maxZ - minZ)) * 0.4
    minP = minPoint rect ^. core
    maxP = maxPoint rect ^. core
    minZ = zHash' minP
    maxZ = zHash' maxP
    zHash' (Point2 a b) = zHash (V2 a b)


seed :: Int
seed = hash "zhashing"

nPoints :: Int
nPoints = 10

genPointF :: (RandomGen g) => Rand g (Point 2 Word)
genPointF = Point2 <$> r <*> r
  where
    r = getRandomR (0, granularity-1)
    granularity = 2^4

genPointsF :: (RandomGen g) => Int -> Rand g [Point 2 Word]
genPointsF n = replicateM n genPointF


boxes :: [Rectangle () Word]
boxes = flip evalRand (mkStdGen seed) $ do
  a <- map boundingBox <$> genPointsF nPoints
  b <- map boundingBox <$> genPointsF nPoints
  return $ zipWith (<>) a b

ppPoint :: Real r => Point 2 r -> SVG
ppPoint (Point2 x y) =
  withFillColorPixel green $
  withStrokeColorPixel black $
  translate (realToFrac x) (realToFrac y) $
  mkCircle nodeRadius
