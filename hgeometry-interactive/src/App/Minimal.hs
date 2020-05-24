{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE UndecidableInstances  #-}
module App.Minimal where

import           Control.Concurrent   (threadDelay)
import           Control.Monad        (forM, guard, void)
import           Control.Monad.Reader (MonadReader (..), runReaderT)
import           Reflex
import           Reflex.SDL2


import qualified SDL
import           SDL (Renderer,Texture)
import           SDL.Cairo (createCairoTexture', withCairoTexture')
import qualified Graphics.Rendering.Cairo.Canvas as Canvas
import           Graphics.Rendering.Cairo.Canvas (Canvas, runCanvas)

--------------------------------------------------------------------------------
-- | An axis aligned bounding box.
data AABB = AABB InputMotion (V2 Int)


--------------------------------------------------------------------------------
-- | Convert a mouse button to an AABB.
mouseButtonToAABB :: MouseButtonEventData -> AABB
mouseButtonToAABB dat = AABB (mouseButtonEventMotion dat) pos
  where P pos32 = mouseButtonEventPos dat
        pos = fromIntegral <$> pos32


--------------------------------------------------------------------------------
-- | Convert a mouse button motion to color.
motionToColor :: InputMotion -> V4 Int
motionToColor Released = V4 255 0 0   128
motionToColor Pressed  = V4 0   0 255 128


--------------------------------------------------------------------------------
-- | Renders an AABB using the handy SDL 2d 'Renderer'.

-- renderAABB :: MonadIO m => Renderer -> V4 Int -> V2 Int -> m ()
-- renderAABB r color pos = do
--   rendererDrawColor r $= (fromIntegral <$> color)
--   fillRect r $ Just $ Rectangle (P $ fromIntegral <$> pos - 10) 20


renderAABB           :: V4 Int -> V2 Int -> Canvas ()
renderAABB color pos = do
                          Canvas.fill $ fromIntegral <$> color
                          Canvas.rect $ Canvas.toD (fromIntegral <$> pos) (V2 20 20)

-------------------------------------------------------------------------------
-- | A type representing one layer in our app.
type Layer m = Performable m (Canvas ())


----------------------------------------------------------------------
-- | Commit a layer stack that changes over time.

commitLayers :: (ReflexSDL2 t m, MonadDynamicWriter t [Layer m] m)
             => Dynamic t [Layer m] -> m ()
commitLayers = tellDyn


----------------------------------------------------------------------
-- | Commit one layer that changes over time.
commitLayer :: (ReflexSDL2 t m, MonadDynamicWriter t [Layer m] m)
            => Dynamic t (Layer m) -> m ()
commitLayer = tellDyn . fmap pure


ffor2       :: Reflex t => Dynamic t a -> Dynamic t b -> (a -> b -> c) -> Dynamic t c
ffor2 a b f = zipDynWith f a b

ffor2up     :: Reflex t => Dynamic t a -> Dynamic t b1 -> ((a, b1) -> b) -> Dynamic t b
ffor2up a b = ffor (zipDyn a b)


data ButtonState = ButtonStateUp
                 | ButtonStateOver
                 | ButtonStateDown
                 deriving Eq


buttonState :: Bool -> Bool -> ButtonState
buttonState isInside isDown
  | not isInside = ButtonStateUp
  | isDown       = ButtonStateDown
  | otherwise    = ButtonStateOver


-- button :: (ReflexSDL2 t m, MonadDynamicWriter t [Layer m] m, MonadReader RenderingEnv m)
--        => m (Event t ButtonState)
-- button = do
--   evMotionData <- getMouseMotionEvent
--   let position = V2 100 100
--       size     = V2 100 100
--       V2 tlx tly = position
--       V2 brx bry = position + size
--       evMotionPos = fmap fromIntegral . mouseMotionEventPos <$> evMotionData
--       evMouseIsInside = ffor evMotionPos $ \(P (V2 x y)) ->
--         (x >= tlx && x <= brx) && (y >= tly && y <= bry)
--   dMouseIsInside <- holdDyn False evMouseIsInside

--   evBtn <- getMouseButtonEvent
--   let evBtnIsDown = ffor evBtn $ (== Pressed) . mouseButtonEventMotion
--   dButtonIsDown <- holdDyn False evBtnIsDown

--   let dButtonStatePre = buttonState <$> dMouseIsInside <*> dButtonIsDown
--   evPB         <- getPostBuild
--   dButtonState <- holdDyn ButtonStateUp $ leftmost [ updated dButtonStatePre
--                                                    , ButtonStateUp <$ evPB
--                                                    ]
--   RenderingEnv r _ <- ask
--   commitLayer $ ffor dButtonState $ \st -> do
--     let color = case st of
--                   ButtonStateUp   -> V4 192 192 192 255
--                   ButtonStateOver -> 255
--                   ButtonStateDown -> V4 128 128 128 255
--     rendererDrawColor r $= color
--     fillRect r $ Just $ Rectangle (P position) size

--   updated <$> holdUniqDyn dButtonState


-- guest :: ( ReflexSDL2 t m
--          , MonadDynamicWriter t [Layer m] m
--          , MonadReader RenderingEnv m
--          )
--       => m ()
-- guest = do


mousePositionEvent = getMouseMotionEvent


mkCanvas :: ReflexSDL2 t m => m (Dynamic t (Canvas ()))
mkCanvas = do
             -- Print some stuff after the network is built.
             evPB <- getPostBuild
             performEvent_ $ ffor evPB $ \() ->
               liftIO $ putStrLn "starting up..."

             ------------------------------------------------------------------------------
             -- A button!
             ------------------------------------------------------------------------------
             -- evBtnState <- button
             -- let evBtnPressed = fmapMaybe (guard . (== ButtonStateDown)) evBtnState
             -- performEvent_ $ ffor evBtnPressed $ const $ liftIO $ putStrLn "Button pressed!"

             ------------------------------------------------------------------------------
             -- Ghosty trail of squares
             ------------------------------------------------------------------------------
             -- Gather all mouse motion events into a list, then commit a commitLayers that
             -- renders each move as a quarter alpha'd yello or cyan square.
             evMouseMove <- getMouseMotionEvent
             dMoves      <- foldDyn (\x xs -> take 100 $ x : xs) [] evMouseMove


             pure $ drawMoves <$> dMoves
  where
    drawMoves  (reverse -> moves) = mapM_ drawSquare moves
    drawSquare dat = let P pos = fromIntegral <$> mouseMotionEventPos dat
                         color = if null (mouseMotionEventState dat)
                                 then V4 255 255 0   128
                                 else V4 0   255 255 128
                     in renderAABB color pos

app :: (ReflexSDL2 t m, MonadReader RenderingEnv m) => m ()
app = do
        canvas <- mkCanvas
        rEnv   <- ask
        performEvent_ $ renderIn rEnv <$> updated canvas

         ------------------------------------------------------------------------------
         -- Quit on a quit event
        ------------------------------------------------------------------------------
        evQuit <- getQuitEvent
        performEvent_ $ liftIO (putStrLn "bye!") <$ evQuit
        shutdownOn =<< delay 0 evQuit

renderIn                                      :: MonadIO m => RenderingEnv -> Canvas () -> m ()
renderIn (RenderingEnv renderer texture) draw =
  liftIO . withCairoTexture' texture . runCanvas $ do
    Canvas.background $ Canvas.rgb 255 255 255
    draw
    SDL.copyEx renderer texture Nothing Nothing 0 Nothing (V2 False False)--(V2 False True)
    SDL.present renderer


-- app :: (ReflexSDL2 t m, MonadReader RenderingEnv m) => m ()
-- app = do
--   (_, dynLayers) <- runDynamicWriterT guest
--   RenderingEnv renderer texture <- ask
--   performEvent_ $ ffor (updated dynLayers) $ \layers -> do
--     canvasLayers <- sequence layers
--     liftIO . withCairoTexture' texture . runCanvas $ do
--       Canvas.background $ Canvas.rgb 255 255 255
--       sequence_ canvasLayers
--       SDL.copyEx renderer texture Nothing Nothing 0 Nothing (V2 False True)
--       SDL.present renderer


  -- performEvent_ $ ffor (updated dynLayers) $ \layers -> do
  --   rendererDrawColor r $= V4 0 0 0 255
  --   clear r
  --   sequence_ layers
  --   present r


data RenderingEnv = RenderingEnv Renderer Texture deriving Eq

main :: IO ()
main = do
  initializeAll
  let ogl            = defaultOpenGL { glProfile = Core Debug 3 3 }
      windowConfig   = defaultWindow { windowOpenGL      = Just ogl
                                     , windowResizable   = True
                                     , windowHighDPI     = True
                                     , windowInitialSize = V2 1280 720
                                     }
      rendererConfig = SDL.defaultRenderer { SDL.rendererType = SDL.SoftwareRenderer
                                           , SDL.rendererTargetTexture = True
                                           }

  window <- createWindow "hgeometry-interactive" windowConfig
  void $ glCreateContext window

  putStrLn "creating renderer..."
  r <- createRenderer window (-1) rendererConfig
  -- create a texture suitable to use cairo on
  t <- createCairoTexture' r window

  -- rendererDrawBlendMode r $= BlendAlphaBlend

  -- Host the network with an example of how to embed your own effects.
  -- In this case it's a simple reader.
  host $ runReaderT app $ RenderingEnv r t
  destroyTexture t
  destroyRenderer r
  destroyWindow window
  quit
