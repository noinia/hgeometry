{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

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



drawE                      :: Gtk.DrawingArea
                           -> (Gtk.DrawingArea -> Canvas a)
                           -> MomentIO (Event a)
drawE drawingArea renderer = signalE1' drawingArea #draw $ \context -> do
    width  <- realToFrac . fromIntegral <$> #getAllocatedWidth  drawingArea
    height <- realToFrac . fromIntegral <$> #getAllocatedHeight drawingArea
    renderCanvas context (V2 width height) (renderer drawingArea)


-- drawE                     :: Gtk.DrawingArea -> Render a -> MomentIO a
-- drawE drawingArea renderer = signalE1' canvas #draw $ \context ->
--                                renderWithContext context (renderer drawingArea)

networkDescription :: MomentIO ()
networkDescription = do
    b <- Gtk.builderNew
    Gtk.builderAddFromFile b "interactive/canvas.ui"

    window   <- castB b "window" Gtk.Window
    destroyE <- signalE0 window #destroy
    reactimate $ Gtk.mainQuit <$ destroyE

    mouseLabel <- castB b "mouseLabel" Gtk.Label

    drawingArea   <- castB b "canvas" Gtk.DrawingArea

    Gtk.widgetAddEvents drawingArea (gflagsToWord [ Gdk.EventMaskPointerMotionMask
                                                  , Gdk.EventMaskButtonPressMask
                                                  ])

    canvasE <- drawE drawingArea updateCanvas'

    mouseMotionE <- signalE1' drawingArea #motionNotifyEvent $ \e -> do
                      x <- Gdk.getEventMotionX e
                      y <- Gdk.getEventMotionY e
                      return (x,y)

    mouseMotionB  <- stepper ""                (showT <$> mouseMotionE)
    sink mouseLabel   [#label :== mouseMotionB]



    mousePressedE <- signalE1' drawingArea #buttonPressEvent $ \e -> do
                      x <- Gdk.getEventButtonX e
                      y <- Gdk.getEventButtonY e
                      return (x,y)
    reactimate $ print <$> mousePressedE



    -- mousePressedB <- stepper "not clicked yet" (showT <$> mousePressedE)

    -- mouseMotionB  <- stepper ""                (showT <$> mouseMotionE)
    -- sink mouseLabel   [#label :== mouseMotionB]



    #showAll window


-- main' :: IO ()
-- main' = do
--     b <- Gtk.builderNew
--     Gtk.builderAddFromFile b "interactive/canvas.ui"

--     window   <- castB b "window" Gtk.Window
--     canvas   <- castB b "canvas" Gtk.DrawingArea

--     Gtk.widgetAddEvents canvas (gflagsToWord [ Gdk.EventMaskPointerMotionMask
--                                              , Gdk.EventMaskButtonPressMask
--                                              ])

--     on canvas #draw $ \context ->
--       renderWithContext context (updateCanvas canvas) >> pure True

--     -- -- mouse motion
--     -- on canvas #motionNotifyEvent $ \e -> do
--     --                                        x <- Gdk.getEventMotionX e
--     --                                        y <- Gdk.getEventMotionY e
--     --                                        print (x,y)
--     --                                        return True

--     on canvas #buttonPressEvent $ \e -> do
--                                           x <- Gdk.getEventButtonX e
--                                           y <- Gdk.getEventButtonY e
--                                           print (x,y)
--                                           return True

--     #showAll window

runGtk = do
    Gtk.init Nothing
    compile networkDescription >>= actuate
    Gtk.main

main :: IO ()
main = runGtk `catch` (\(e::Gtk.GError) -> Gtk.gerrorMessage e >>= putStrLn . T.unpack)

updateCanvas'             :: Gtk.DrawingArea -> Canvas ()
updateCanvas' drawingArea = do
    Canvas.background $ Canvas.rgb 255 255 255
    Canvas.stroke $ Canvas.gray 10
    Canvas.fill $ Canvas.red 255
    Canvas.line (V2 30 30) (V2 400 100)
    let frames = 20

    Canvas.line (V2 10 10) (V2 (25+frames) (25+frames))


updateCanvas        :: Gtk.DrawingArea -> Render ()
updateCanvas canvas = do
    width'  <- fromIntegral <$> #getAllocatedWidth  canvas
    height' <- fromIntegral <$> #getAllocatedHeight canvas
    let width  = realToFrac width'
        height = realToFrac height'

    setSourceRGB 1 0 0
    setLineWidth 20
    setLineCap LineCapRound
    setLineJoin LineJoinRound

    moveTo 30 30
    lineTo (width-30) (height-30)
    lineTo (width-30) 30
    lineTo 30 (height-30)
    stroke

    setSourceRGB 1 1 0
    setLineWidth 4

    save
    translate (width / 2) (height / 2)
    scale (width / 2) (height / 2)
    arc 0 0 1 (135 * pi/180) (225 * pi/180)
    restore
    stroke

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
