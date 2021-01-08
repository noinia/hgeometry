{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
module Main (main) where

import           Control.Lens
import qualified Data.Foldable             as F
import           Data.List                 (nub, transpose)
import qualified Data.Vector               as V
import qualified Data.Vector.Circular      as CV
import qualified Data.Vector.Circular.Util as CV
import qualified Data.Vector.Unboxed       as VU
import           Linear.V2                 (V2 (V2))
import           Linear.Vector             (Additive (lerp))
import           Reanimate
import           Reanimate.Animation       (Sync (SyncFreeze))

import Algorithms.Geometry.SSSP
import Data.Ext
import Data.Geometry.Point
import Data.Geometry.Polygon

import Common

{- GIFs:
 1. SSSP from a single point.
 2. SSSP from all points.
 3. Morph to tree.
-}

main :: IO ()
main = reanimate $
  mapA (withViewBox (screenBottom, screenBottom, screenHeight, screenHeight)) $
  scene $ do
    newSpriteSVG_ $ mkBackground bgColor
    -- play $ ssspSingle
    play $ ssspMulti
    -- play $ ssspMorph
    -- wait 1

targetPolygon :: SimplePolygon () Rational
targetPolygon = pScale 2 $ pAtCenter $ simpleFromPoints $ map ext
  [ Point2 0 0
  , Point2 1 0
  , Point2 1 1, Point2 2 1, Point2 2 (-1)
  , Point2 0 (-1), Point2 0 (-2)
  , Point2 3 (-2), Point2 3 2, Point2 0 2 ]

ssspSingle :: Animation
ssspSingle = pauseAtEnd 1 $ scene $ do
    newSpriteSVG_ $ ppPolygonBody grey targetPolygon
    newSpriteSVG_ $ ppPolygonOutline black targetPolygon
    nodes <- newSpriteSVG $ mkGroup
      [ ppPolygonNodes targetPolygon
      , withFillColorPixel rootColor $  ppPolygonNode targetPolygon 0 ]
    spriteZ nodes 1
    case t of
      T idx sub -> mapM_ (worker idx) sub
    wait 3
  where
    worker origin (T target sub) = do
      let Point2 oX oY = realToFrac <$> targetPolygon ^. outerVertex origin . core
          Point2 tX tY = realToFrac <$> targetPolygon ^. outerVertex target . core
      fork $ do
        wait 0.7
        dot <- fork $ newSpriteA' SyncFreeze $ animate $ \t -> mkGroup $
          [ translate tX tY $
            withFillColorPixel pathColor $ mkCircle (fromToS 0 (nodeRadius/2) t)
          ]
        spriteZ dot 2
        spriteE dot (overEnding 0.2 fadeOutE)
      l <- newSpriteA' SyncFreeze $ animate $ \t -> mkGroup $
        [ withStrokeColorPixel pathColor $ partialSvg t $
          pathify $ mkLine (oX, oY) (tX, tY)
        , translate tX tY $
          withFillColorPixel pathColor $ mkCircle (fromToS 0 (nodeRadius/2) t)
        ]
      spriteE l (overEnding 0.2 fadeOutE)
      mapM_ (worker target) sub
    t = ssspTree targetPolygon

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

ssspMorph :: Animation
ssspMorph =
    playThenReverseA $ pauseAround 1 3 $ signalA (curveS 2) $ mkAnimation 2 $ \t ->
      ppPolyGroup t (lerpPolygon t targetPolygon mPolygon)
  where
    ppPolyGroup t p = mkGroup
        [ --withGroupOpacity (max 0.3 (1-t)) $
          ppPolygonBody grey p
        , ppPolygonOutline black p
        , ppSSSP' tree p
        , ppPolygonNodes p
        , withFillColorPixel rootColor $  ppPolygonNode p 0
        ]
    p' = fromPoints $ toPoints $ targetPolygon
    tree = sssp (triangulate p')
    mPolygon = pScale 2 $ morphSSSP targetPolygon
