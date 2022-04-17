{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module FastVisibility (fastVisibilityShowcase) where

import Control.Lens
import Control.Monad
import Reanimate
import Reanimate.Animation

import           Algorithms.Geometry.PolygonTriangulation.Triangulate (computeDiagonals)
import           Algorithms.Geometry.PolygonTriangulation.Types
import           Algorithms.Geometry.SSSP                             (triangulate, visibilityDual,
                                                                       visibilityFinger)
import           Data.Ext
import           Geometry.PlanarSubdivision                      (PolygonFaceData)
import           Geometry.Point
import           Geometry.Polygon
import           Geometry.Transformation
import           Data.PlaneGraph
import qualified Data.Vector                                          as V
import           Graphics.SvgTree                                     (LineJoin (..), Cap(..),strokeLineCap)

import Common
import Data.RealNumber.Rational

type R = RealNumber 5

targetPolygon :: SimplePolygon () R
targetPolygon = scaleUniformlyBy 2 $ pAtCenter $ simpleFromPoints $ map ext
  [ Point2 0 0
  , Point2 1 0
  , Point2 1 1, Point2 2 1, Point2 2 (-1)
  , Point2 0 (-1), Point2 0 (-2)
  , Point2 3 (-2), Point2 3 2, Point2 0 2 ]

targetTriangulation = triangulate targetPolygon

fastVisibilityShowcase :: Animation
fastVisibilityShowcase = scene $ do
  let p = targetPolygon
      t = triangulate p
  newSpriteSVG_ $ mkGroup
    [ -- ppPolygonBody grey p
      -- ppTriangulation p
      ppPolygonOutline black p
    , ppPolygonNodes nodeColor p
    , withFillColorPixel rootColor $ ppPolygonNode nodeColor p 0
    ]
  tSprite <- adjustZ pred $ newSpriteA' SyncFreeze $ animateTriangulation p
  spriteE tSprite $ overEnding 0.2 fadeOutE
  wait 1
  path <- newPath
  forM_ (visibilityFinger (visibilityDual t)) $ \event -> do
    case event of
      Left (a,b,c) -> do
        s <- adjustZ (subtract 2) $ newSpriteA' SyncFreeze $ animate $ \t ->
          withGroupOpacity t $
          ppPolygonBody grey $ fromPoints [p^.outerVertex a, p^.outerVertex b, p^.outerVertex c]
        spriteE s $ overEnding 0.2 fadeOutE
      Right pt -> pushPath path pt
    wait 0.5
  pushPath path (p^.outerVertex 0.core)
  wait 2

newPath :: Scene s (Var s [Point 2 R])
newPath = do
    path <- newVar []
    s <- newSprite $ render <$> unVar path
    spriteE s $ overEnding 0.2 fadeOutE
    return path
  where
    render lst = withFillOpacity 0 $ withStrokeColorPixel green $
      withStrokeLineJoin JoinRound $ (strokeLineCap .~ pure CapRound) $
      mkLinePath
      [ (x,y) | e <- lst, let Point2 x y = realToFrac <$> e ]

pushPath :: Var s [Point 2 R] -> Point 2 R -> Scene s ()
pushPath var pt = do
  oldPath <- readVar var
  if null oldPath
    then writeVar var [pt]
    else tweenVar var 1 $ \_ t ->
            oldPath ++ [lerpPoint (curveS 2 t) pt (last oldPath)]

animateTriangulation :: (Fractional r, Real r, Ord r) => SimplePolygon p r -> Animation
animateTriangulation p = scene $ do
  forM_ (computeDiagonals p) $ \lineSegment -> do
    fork $ newSpriteA' SyncFreeze $ animate (\t ->
      partialSvg t $ pathify $
        withStrokeColorPixel red $
        ppLineSegment lineSegment)
      & signalA (curveS 2)
    wait 0.2

ppTriangulation :: (Fractional r, Real r, Ord r) => SimplePolygon p r -> SVG
ppTriangulation p = mkGroup
  [ withStrokeWidth (defaultStrokeWidth) $
    withStrokeColorPixel red $
    ppLineSegment lineSegment
  | lineSegment <- computeDiagonals p
  ]
