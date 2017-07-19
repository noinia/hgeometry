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
         SDL.initialize [SDL.InitVideo]
         w <- SDL.createWindow "sdl2-gfx-example" SDL.defaultWindow
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


      SDL.present r
      SDL.Framerate.delay_ fpsm -- Delay to keep framerate constant.

      let doQuit = not . null . filter (isKeyPress SDL.KeycodeQ) $ events

      unless doQuit loop'


isKeyPress c e = case SDL.eventPayload e of
                   SDL.KeyboardEvent ke -> SDL.keyboardEventKeyMotion ke == SDL.Pressed
                                        && SDL.keyboardEventKeysym ke    == c
                   _                    -> False
