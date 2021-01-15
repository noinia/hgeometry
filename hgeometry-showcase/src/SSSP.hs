{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module SSSP (ssspMulti) where

import Control.Lens        ((^.))
import Reanimate           (Animation, animate, curveS, fadeOutE, fork, fromToS, mkAnimation,
                            mkCircle, mkGroup, mkLine, newSpriteA', newSpriteSVG, newSpriteSVG_,
                            overEnding, partialSvg, pathify, pauseAround, pauseAtEnd,
                            playThenReverseA, scene, signalA, spriteE, spriteZ, translate, wait,
                            withFillColorPixel, withStrokeColorPixel)
import Reanimate.Animation (Sync (SyncFreeze))

import Algorithms.Geometry.SSSP     (sssp, triangulate)
import Data.Ext                     (core, ext)
import Data.Geometry.Point          (Point (Point2))
import Data.Geometry.Polygon        (SimplePolygon, fromPoints, outerVertex, simpleFromPoints,
                                     toPoints)
import Data.Geometry.Transformation

import Common

targetPolygon :: SimplePolygon () Rational
targetPolygon = scaleUniformlyBy 2 $ pAtCenter $ simpleFromPoints $ map ext
  [ Point2 0 0
  , Point2 1 0
  , Point2 1 1, Point2 2 1, Point2 2 (-1)
  , Point2 0 (-1), Point2 0 (-2)
  , Point2 3 (-2), Point2 3 2, Point2 0 2 ]

_ssspSingle :: Animation
_ssspSingle = pauseAtEnd 1 $ scene $ do
    newSpriteSVG_ $ ppPolygonBody grey targetPolygon
    newSpriteSVG_ $ ppPolygonOutline black targetPolygon
    nodes <- newSpriteSVG $ mkGroup
      [ ppPolygonNodes targetPolygon
      , withFillColorPixel rootColor $  ppPolygonNode targetPolygon 0 ]
    spriteZ nodes 1
    case tree of
      T idx sub -> mapM_ (worker idx) sub
    wait 3
  where
    worker origin (T target sub) = do
      let Point2 oX oY = realToFrac <$> targetPolygon ^. outerVertex origin . core
          Point2 tX tY = realToFrac <$> targetPolygon ^. outerVertex target . core
      fork $ do
        wait 0.7
        dot <- fork $ newSpriteA' SyncFreeze $ animate $ \t -> mkGroup
          [ translate tX tY $
            withFillColorPixel pathColor $ mkCircle (fromToS 0 (nodeRadius/2) t)
          ]
        spriteZ dot 2
        spriteE dot (overEnding 0.2 fadeOutE)
      l <- newSpriteA' SyncFreeze $ animate $ \t -> mkGroup
        [ withStrokeColorPixel pathColor $ partialSvg t $
          pathify $ mkLine (oX, oY) (tX, tY)
        , translate tX tY $
          withFillColorPixel pathColor $ mkCircle (fromToS 0 (nodeRadius/2) t)
        ]
      spriteE l (overEnding 0.2 fadeOutE)
      mapM_ (worker target) sub
    tree = ssspTree targetPolygon

ssspMulti :: Animation
ssspMulti = mkAnimation 20 $ \t ->
  let p = addPointAt t targetPolygon in
  mkGroup
  [ ppPolygonBody grey p
  , ppPolygonOutline black p
  , ppSSSP p
  , ppPolygonNodes p
  , withFillColorPixel rootColor $ ppPolygonNode p 0
  ]

_ssspMorph :: Animation
_ssspMorph =
    playThenReverseA $ pauseAround 1 3 $ signalA (curveS 2) $ mkAnimation 2 $ \t ->
      ppPolyGroup t (lerpPolygon t targetPolygon mPolygon)
  where
    ppPolyGroup _t p = mkGroup
        [ --withGroupOpacity (max 0.3 (1-t)) $
          ppPolygonBody grey p
        , ppPolygonOutline black p
        , ppSSSP' tree p
        , ppPolygonNodes p
        , withFillColorPixel rootColor $  ppPolygonNode p 0
        ]
    p' = fromPoints $ toPoints targetPolygon
    tree = sssp (triangulate p')
    mPolygon = scaleUniformlyBy 2 $ morphSSSP targetPolygon
