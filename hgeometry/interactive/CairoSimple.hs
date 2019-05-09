{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Data.IORef
import           Control.Exception (catch)
import           Control.Monad (forM_)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Trans.Reader (runReaderT)
import           Data.GI.Base
import qualified Data.Text as T
import           Foreign.Ptr (castPtr)
import qualified GI.Cairo as GI.Cairo
import qualified GI.GLib as GLib
import qualified GI.Gtk as Gtk
import qualified GI.Gdk as Gdk
import           Graphics.Rendering.Cairo
import qualified Graphics.Rendering.Cairo.Internal as Cairo.Internal
import           Graphics.Rendering.Cairo.Types (Cairo(Cairo))
import           Reactive.Banana
import           Reactive.Banana.Frameworks
import           Reactive.Banana.GI.Gtk
import           Data.GI.Base.Signals(SignalInfo, HaskellCallbackType)

import Data.Geometry.Point

import qualified SDL.Cairo.Canvas as Canvas
import SDL.Cairo.Canvas(Canvas)
import Linear.V2(V2(..))
--------------------------------------------------------------------------------

-- | This function bridges gi-cairo with the hand-written cairo
-- package. It takes a `GI.Cairo.Context` (as it appears in gi-cairo),
-- and a `Render` action (as in the cairo lib), and renders the
-- `Render` action into the given context.
renderWithContext      :: MonadIO m => GI.Cairo.Context -> Render a -> m a
renderWithContext ct r = liftIO $ withManagedPtr ct $ \p ->
                         runReaderT (Cairo.Internal.runRender r) (Cairo (castPtr p))

renderCanvas           :: MonadIO m => GI.Cairo.Context -> V2 Double -> Canvas a -> m a
renderCanvas ct size c = liftIO $ Canvas.withRenderer (renderWithContext ct) size c

showT :: Show a => a -> T.Text
showT = T.pack . show


-- drawE                     :: Gtk.DrawingArea -> Render a -> MomentIO a
-- drawE drawingArea renderer = signalE1' canvas #draw $ \context ->
--                                renderWithContext context (renderer drawingArea)


main' :: IO ()
main' = do
    buttonState <- newIORef True

    b <- Gtk.builderNew
    Gtk.builderAddFromFile b "interactive/canvas.ui"

    window      <- castB b "window" Gtk.Window
    on window #destroy Gtk.mainQuit

    drawingArea <- castB b "canvas" Gtk.DrawingArea

    Gtk.widgetAddEvents drawingArea (gflagsToWord [ Gdk.EventMaskPointerMotionMask
                                                  , Gdk.EventMaskButtonPressMask
                                                  ])

    on drawingArea #draw $ \context -> do
      width  <- realToFrac . fromIntegral <$> #getAllocatedWidth  drawingArea
      height <- realToFrac . fromIntegral <$> #getAllocatedHeight drawingArea
      b <- readIORef buttonState
      renderCanvas context (V2 width height) (updateCanvas' b drawingArea)
      pure True

    -- -- mouse motion
    -- on drawingArea #motionNotifyEvent $ \e -> do
    --                                        x <- Gdk.getEventMotionX e
    --                                        y <- Gdk.getEventMotionY e
    --                                        print (x,y)
    --                                        return True

    on drawingArea #buttonPressEvent $ \e -> do
                                          x <- Gdk.getEventButtonX e
                                          y <- Gdk.getEventButtonY e
                                          print (x,y)
                                          return True

    colorButton       <- castB b "colorButton" Gtk.Button
    on colorButton #clicked $ do
                                b <- readIORef buttonState
                                #queueDraw drawingArea
                                writeIORef buttonState (not b)


    #showAll window

runGtk = do
    Gtk.init Nothing
    main'
    Gtk.main

main :: IO ()
main = runGtk `catch` (\(e::Gtk.GError) -> Gtk.gerrorMessage e >>= putStrLn . T.unpack)

updateCanvas' True  = updateCanvas
updateCanvas' False = drawBlue

drawBlue   :: Gtk.DrawingArea -> Canvas ()
drawBlue _ = do
    Canvas.background $ Canvas.blue 100

updateCanvas             :: Gtk.DrawingArea -> Canvas ()
updateCanvas drawingArea = do
    Canvas.background $ Canvas.rgb 255 255 255
    Canvas.stroke $ Canvas.gray 10
    Canvas.fill $ Canvas.red 255
    Canvas.line (V2 30 30) (V2 400 100)
    let frames = 20

    Canvas.line (V2 10 10) (V2 (25+frames) (25+frames))

-- main' = do
--     window <- new Gtk.Window [ #title          := "Haskell-gi Cairo"
--                              , #defaultWidth   := 800
--                              , #defaultHeight  := 600
--                              ]
--     canvas <- Gtk.drawingAreaNew
--     -- on canvas Gtk.sizeRequest $ return (Gtk.Requisition 800 600)

--     set window [Gtk.containerChild := canvas]

--     on canvas #draw $ \context -> do
--       width <- fromIntegral  <$> #getAllocatedWidth  window
--       height <- fromIntegral <$> #getAllocatedHeight window
--       renderWithContext context (updateCanvas canvas width height)
--       return True

--     #showAll window

-- main = do
--     _ <- Gtk.init Nothing
--     main'
--     Gtk.main
