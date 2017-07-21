{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Lens
import           Control.Monad (unless, when)
import           Data.Ext
import           Data.Geometry
import qualified Data.Geometry.Polygon as Polygon
import           Data.Geometry.Polygon (SimplePolygon,outerBoundaryEdges)
import           Data.Maybe (mapMaybe)
import qualified Data.Vector.Storable as V
import           Foreign.C.Types (CInt)
import qualified SDL
import           SDL (($=))
import qualified SDL.Framerate
import qualified SDL.Primitive
import           SDL.Vect (V2(..), V4(..))
import qualified SDL.Video.OpenGL as OpenGL

main :: IO ()
main = do
         SDL.initialize [SDL.InitVideo, SDL.InitEvents]
         let windowConfig = SDL.defaultWindow {
                   SDL.windowHighDPI     = True
                 , SDL.windowOpenGL      = Just $ OpenGL.defaultOpenGL { OpenGL.glMultisampleSamples = 8 }
                 , SDL.windowInitialSize = V2 1280 800
               }

         w <- SDL.createWindow "sdl2 example" windowConfig
         r <- SDL.createRenderer w (-1) SDL.defaultRenderer
         SDL.showWindow w
         let fps   = 60  -- How fast do we aim to render?
         SDL.Framerate.with fps (main' r)

         SDL.destroyWindow w
         SDL.quit
  where

main'        :: SDL.Renderer -> SDL.Framerate.Manager -> IO ()
main' r fpsm = loop'
  where
    loop' = do
      events <- SDL.pollEvents
      SDL.rendererDrawColor r $= V4 255 255 255 255 -- white
      SDL.clear r

      let pg = Polygon.fromPoints . map ext $ [ Point2 100 30
                                              , Point2 220 70
                                              , Point2 50 500                                                                                             ]
      drawPolygon r pg

      let frames = 50
      SDL.Primitive.line r (V2 10 10) (V2 (25+frames) (25+frames)) (V4 0 0 0 255)

      SDL.present r
      SDL.Framerate.delay_ fpsm -- Delay to keep framerate constant.

      let mClicks = mapMaybe mouseClick events
      when (not . null $ mClicks) $ do
        print $ head mClicks


      let doQuit = not . null . filter (isKeyPress SDL.KeycodeQ) $ events

      unless doQuit loop'


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
drawPolygon r pg = SDL.Primitive.fillPolygon r xs ys c
  where
    c  = V4 200 20 20 128
    xs = V.fromList $ pg^..outerBoundary.traverse.core.xCoord
    ys = V.fromList $ pg^..outerBoundary.traverse.core.yCoord
