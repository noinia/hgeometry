{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Lens
import           Control.Monad (unless, when)
import           Data.Ext
import           Data.Geometry
import           Data.Geometry.Polygon (SimplePolygon,outerBoundaryEdges)
import qualified Data.Geometry.Polygon as Polygon
import           Data.Maybe (mapMaybe)
import qualified Data.Vector.Storable as V
import           Foreign.C.Types (CInt)
import qualified SDL
import           SDL (Renderer,Texture)
import           SDL.Cairo (createCairoTexture')
import qualified SDL.Cairo.Canvas as Cairo
import           SDL.Cairo.Canvas (Canvas)



-- import qualified SDL.Framerate
-- import qualified SDL.Primitive
import           SDL.Vect (V2(..), V4(..))
import qualified SDL.Video.OpenGL as OpenGL
-- import qualified SDL.Video.Renderer as Renderer
-- import qualified SDL.Vect as SV

main :: IO ()
main = do
         SDL.initialize [SDL.InitVideo, SDL.InitEvents]
         let windowConfig = SDL.defaultWindow {
                   SDL.windowHighDPI     = True
                 , SDL.windowInitialSize = V2 1280 800
               }
             rendererConfig = SDL.defaultRenderer { SDL.rendererType = SDL.SoftwareRenderer
                                                  , SDL.rendererTargetTexture = True
                                                  }
         -- SDL.setHintWithPriority SDL.OverridePriority SDL.HintRenderScaleQuality SDL.ScaleLinear

         w <- SDL.createWindow "sdl2 example" windowConfig
         r <- SDL.createRenderer w (-1) rendererConfig

         -- create a texture suitable to use cairo on
         t <- createCairoTexture' r w

         main' r t
         -- SDL.showWindow w


         -- let fps   = 60  -- How fast do we aim to render?
         -- SDL.Framerate.with fps (main' r)

         -- SDL.destroyWindow w
         -- SDL.quit



main'                 :: Renderer -> Texture -> IO ()
main' renderer texture = loop' 0
  where
    loop' frames = do
      events <- SDL.pollEvents

      -- draw on the texture
      Cairo.withCanvas texture $ do
        Cairo.background $ Cairo.rgb 255 255 255
        Cairo.stroke $ Cairo.gray 10
        Cairo.fill $ Cairo.red 255
        Cairo.line (V2 30 30) (V2 400 100)

        Cairo.line (V2 10 10) (V2 (25+frames) (25+frames))

        let pg = Polygon.fromPoints . map ext $ [ Point2 100 30
                                                , Point2 220 70
                                                , Point2 50 500
                                                , Point2 55 300
                                                , Point2 30 100
                                                ]
        drawPolygon renderer pg

      -- apply texture and show on screen
      SDL.copyEx renderer texture Nothing Nothing 0 Nothing (V2 False True)
      SDL.present renderer
      -- slow down to approx. 120 FPS
      SDL.delay (1000 `div` 120)

      let mClicks = mapMaybe mouseClick events
      when (not . null $ mClicks) $ do
        print $ head mClicks

      let doQuit = not . null . filter (isKeyPress SDL.KeycodeQ) $ events
      unless doQuit (loop' $ frames + 1)


-- main'        :: SDL.Renderer -> SDL.Framerate.Manager -> IO ()
-- main' r fpsm = loop'
--   where
--     loop' = do
--       events <- SDL.pollEvents
--       SDL.rendererDrawColor r $= V4 255 255 255 255 -- white
--       SDL.clear r

--       let pg = Polygon.fromPoints . map ext $ [ Point2 100 30
--                                               , Point2 220 70
--                                               , Point2 50 500
--                                               ]
--       drawPolygon r pg

--       let frames = 50

--       SDL.Primitive.smoothLine r (V2 10 10) (V2 (25+frames) (25+frames)) (V4 0 0 0 255)

--       SDL.Primitive.smoothLine r (V2 400 30) (V2 350 500) (V4 0 0 0 255)

--       Renderer.drawLine r (SV.P $ V2 600 30) (SV.P $ V2 550 500)

--       SDL.present r
--       SDL.Framerate.delay_ fpsm -- Delay to keep framerate constant.

--       let mClicks = mapMaybe mouseClick events
--       when (not . null $ mClicks) $ do
--         print $ head mClicks


--       let doQuit = not . null . filter (isKeyPress SDL.KeycodeQ) $ events

--       unless doQuit loop'


mouseClick e = case SDL.eventPayload e of
                 SDL.MouseButtonEvent mbe |  SDL.mouseButtonEventMotion mbe == SDL.Pressed
                                          && SDL.mouseButtonEventButton mbe == SDL.ButtonLeft
                                               -> Just $ SDL.mouseButtonEventPos mbe
                 _                             -> Nothing




isKeyPress c e = case SDL.eventPayload e of
                   SDL.KeyboardEvent ke -> SDL.keyboardEventKeyMotion ke                     == SDL.Pressed
                                        && SDL.keysymKeycode (SDL.keyboardEventKeysym ke)    == c
                   _                    -> False


-- drawPolygon      :: MonadIO m => SDL.Renderer -> SimplePolygon p Int16 -> m ()
drawPolygon r pg = Cairo.polygon $ pg^..outerBoundary.traverse.core.vector.to toV2
