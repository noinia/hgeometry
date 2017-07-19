module Main where

import Control.Monad(unless)
import SDL.Vect             (V2(..), V4(..))
import SDL                  (($=))
import Foreign.C.Types (CInt)

import qualified SDL
import qualified SDL.Framerate
import qualified SDL.Primitive

main :: IO ()
main = do
         SDL.initialize [SDL.InitVideo, SDL.InitEvents]
         w <- SDL.createWindow "sdl2 example" SDL.defaultWindow
         r <- SDL.createRenderer w (-1) SDL.defaultRenderer
         SDL.showWindow w
         let fps   = 60  -- How fast do we aim to render?
         SDL.Framerate.with fps main'

         SDL.destroyWindow w
         SDL.quit
  where

main'        :: SDL.Renderer -> SDL.Framerate.Manager -> IO ()
main' r fpsm = loop'
  where
    loop' = do
      events <- SDL.pollEvents
      -- SDL.rendererDrawColor r $= black
      SDL.clear r

      let pg = Polygon.fromList . map ext $ [Point2 10 10, Point2 50 50, Point2 100 100]
      drawPolygon r pg

      SDL.present r
      SDL.Framerate.delay_ fpsm -- Delay to keep framerate constant.

      let doQuit = not . null . filter (isKeyPress SDL.KeycodeQ) $ events

      unless doQuit loop'


isKeyPress c e = case SDL.eventPayload e of
                   SDL.KeyboardEvent ke -> SDL.keyboardEventKeyMotion ke == SDL.Pressed
                                        && SDL.keyboardEventKeysym ke    == c
                   _                    -> False


drawPolygon      :: MonadIO m => SDL.Renderer -> SimplePolygon p Int16 -> m ()
drawPolygon r pg = SDL.Primitive.fillPolygon r xs ys c
  where
    c  = V4 0 0 0 0 -- black
    xs = pg^..outerBoundary.core.xCoord
    ys = pg^..outerBoundary.core.yCoord
