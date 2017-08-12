{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Control.Exception (catch)
import           Control.Monad (forM_)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Trans.Reader (runReaderT)
import           Data.GI.Base
import           Data.GI.Base.Signals (SignalInfo, HaskellCallbackType)
import           Data.IORef
import qualified Data.Text as T
import           Foreign.Ptr (castPtr)
import qualified GI.Cairo as GI.Cairo
import qualified GI.GLib as GLib
import qualified GI.Gdk as Gdk
import qualified GI.Gtk as Gtk
import           Graphics.Rendering.Cairo
import qualified Graphics.Rendering.Cairo.Internal as Cairo.Internal
import           Graphics.Rendering.Cairo.Types (Cairo(Cairo))
import           Reactive.Banana
import           Reactive.Banana.Frameworks
import           Reactive.Banana.GI.Gtk

import           Data.Geometry.Point

import qualified SDL.Cairo.Canvas as Canvas
import           SDL.Cairo.Canvas (Canvas)
import           Linear.V2 (V2(..))
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

-- | Get an 'Reactive.Banana.Event' from
-- a 'Data.GI.Base.Signals.SignalProxy' that produces one argument.
signalE1'
    ::
        ( HaskellCallbackType info ~ (a -> IO Bool)
        , SignalInfo info
        , Gtk.GObject self
        )
    => self
    -> SignalProxy self info
    -> (a -> IO b) -- ^ function to transform the Event with
    -> MomentIO (Event b)
signalE1' self signal h = signalEN self signal f >>= mapEventIO h
  where
    f g = \a -> g a >> return True -- we return True because the event has been
                                   -- handled, don't want to propagate it
                                   -- further

newtype MouseMotionData = MouseMotionData (Point 2 Double)
                          deriving (Show,Eq,Ord)





draw                :: Gtk.DrawingArea -> Behavior (Canvas ()) -> MomentIO ()
draw drawingArea bc = do
    canvasRef <- liftIO . newIORef =<< valueB bc -- gets the initial canvas

    -- set up reactive-banana to update the canvasRef on changes, and triger a
    -- redraw
    c <- valueBLater bc
    liftIOLater $ writeIORef canvasRef c
    e <- changes bc
    reactimate' $ (fmap $ \c' -> do liftIO $ writeIORef canvasRef c'
                                    #queueDraw drawingArea
                  ) <$> e

    -- registers drawing event handler
    _ <- on drawingArea #draw $ \context -> do
        w <- realToFrac . fromIntegral <$> #getAllocatedWidth  drawingArea
        h <- realToFrac . fromIntegral <$> #getAllocatedHeight drawingArea
        canvas <- readIORef canvasRef
        renderCanvas context (V2 w h) canvas
        pure True
    pure ()



-- openFileDialog   :: Gtk.Builder -> MomentIO FilePath
-- openFileDialog b = do
--     fileChooser <- castB b "fileChooser" Gtk.FileChooserDialog

--     cancelButton <- castB b "openFile" Gtk.Button
--     cancelE      <- signalE0 cancelButton #clicked
--     reactimate $ (#hideAll fileChooser) <$ cancelE

--     openButton       <- castB b "openFile" Gtk.Button
--     pressedE          <- signalE0 openButton #clicked
--     reactimate'





networkDescription :: MomentIO ()
networkDescription = do
    b <- Gtk.builderNew
    Gtk.builderAddFromFile b "interactive/canvas.ui"

    window   <- castB b "window" Gtk.Window
    destroyE <- signalE0 window #destroy
    reactimate $ Gtk.mainQuit <$ destroyE

    quitMenuItem <- castB b "quitMenuItem" Gtk.ImageMenuItem
    quitClickE <- signalE0 quitMenuItem #activate
    reactimate $ Gtk.mainQuit <$ quitClickE

    openFileMenuItem <- castB b "openFileMenuItem" Gtk.ImageMenuItem
    openFileClickE   <- signalE0 openFileMenuItem #activate



    mouseLabel <- castB b "mouseLabel" Gtk.Label

    colorButton       <- castB b "colorButton" Gtk.Button
    pressedE          <- signalE0 colorButton #clicked
    colorButtonStateB <- accumB False (not <$ pressedE)



    drawingArea   <- castB b "canvas" Gtk.DrawingArea

    Gtk.widgetAddEvents drawingArea (gflagsToWord [ Gdk.EventMaskPointerMotionMask
                                                  , Gdk.EventMaskButtonPressMask
                                                  ])


    -- canvasE <- drawE drawingArea updateCanvas'

    mouseMotionE <- signalE1' drawingArea #motionNotifyEvent $ \e -> do
                      x <- Gdk.getEventMotionX e
                      y <- Gdk.getEventMotionY e
                      return $! V2 x y

    mouseMotionB  <- stepper (V2 0 0) mouseMotionE
    -- sink mouseLabel   [#label :== mouseMotionB]


    draw drawingArea (followMouse mouseMotionB)



    mousePressedE <- signalE1' drawingArea #buttonPressEvent $ \e -> do
                      x <- Gdk.getEventButtonX e
                      y <- Gdk.getEventButtonY e
                      return (x,y)
    reactimate $ print <$> mousePressedE



    -- mousePressedB <- stepper "not clicked yet" (showT <$> mousePressedE)

    -- mouseMotionB  <- stepper ""                (showT <$> mouseMotionE)
    -- sink mouseLabel   [#label :== mouseMotionB]



    #showAll window


runGtk = do
    Gtk.init Nothing
    compile networkDescription >>= actuate
    Gtk.main

main :: IO ()
main = runGtk `catch` (\(e::Gtk.GError) -> Gtk.gerrorMessage e >>= putStrLn . T.unpack)


followMouse :: Behavior (V2 Double) -> Behavior (Canvas ())
followMouse = fmap $ \pos -> do
    Canvas.background $ Canvas.blue 255
    Canvas.stroke $ Canvas.red 250
    Canvas.circle' pos 100



updateCanvas :: Behavior Bool -> Behavior (Canvas ())
updateCanvas = fmap f
  where
    f True  = updateCanvas'
    f False = drawBlue



updateCanvas'             :: Canvas ()
updateCanvas'  = do
    Canvas.background $ Canvas.rgb 255 255 255
    Canvas.stroke $ Canvas.gray 10
    Canvas.fill $ Canvas.red 255
    Canvas.line (V2 30 30) (V2 400 100)
    let frames = 20

    Canvas.line (V2 10 10) (V2 (25+frames) (25+frames))

    img <- Canvas.grab (Canvas.D 0 0 300 300)
    Canvas.image img (V2 350 0)





drawBlue   :: Canvas ()
drawBlue = do
    Canvas.background $ Canvas.blue 250
