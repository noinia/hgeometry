{-# LANGUAGE OverloadedLabels #-}
module RenderUtil where

import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Trans.Reader (runReaderT)
import           Data.GI.Base
import           Data.GI.Base.Signals (SignalInfo, HaskellCallbackType)
import           Data.IORef
import qualified Data.Text as T
import           Foreign.Ptr (castPtr)
import qualified GI.Cairo as GI.Cairo
import qualified GI.Gtk as Gtk
import           Graphics.Rendering.Cairo
import           Graphics.Rendering.Cairo.Canvas (Canvas)
import qualified Graphics.Rendering.Cairo.Canvas as Canvas
import qualified Graphics.Rendering.Cairo.Internal as Cairo.Internal
import           Graphics.Rendering.Cairo.Types (Cairo(Cairo))
import           Linear.V2 (V2(..))
import           Reactive.Banana
import           Reactive.Banana.Frameworks
import           Reactive.Banana.GI.Gtk




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
