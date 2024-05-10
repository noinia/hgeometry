{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
module CanvasKit
  ( CanvasKit
  , ckAsJSVal


  , Surface
  , SkCanvasRef


  , InitializeSkCanvasAction(..)
  , initializeCanvasKitSub

  , requestAnimationFrame
  ) where

import           Control.Lens
import           Control.Monad.IO.Class
import qualified Data.Map as Map
import           GHCJS.Marshal (fromJSVal, ToJSVal(..))
import           GHCJS.Types
import           HGeometry.Point
import           HGeometry.Vector
import           HGeometry.Viewport
import           Language.Javascript.JSaddle.Object (jsg1, jsg2, jsf, js1, jsg)
import qualified Language.Javascript.JSaddle.Object as JS
import           Miso
import           Miso.String (MisoString)
import           MouseExtra

--------------------------------------------------------------------------------
-- * The CanvasKit object

 -- ^ the CanvasKit object
newtype CanvasKit = MkCanvasKit { ckAsJSVal :: JSVal}
  deriving newtype (JS.MakeObject,ToJSVal)

instance Show CanvasKit where
  show _ = "CanvasKitObj"
instance Eq CanvasKit where
  _ == _ = True
  -- we should only have one

--------------------------------------------------------------------------------
-- * Surface

 -- ^ the Surface object
newtype Surface = MkSurface JSVal
  deriving newtype (JS.MakeObject, ToJSVal)

instance Show Surface where
  show _ = "SurfaceObj"
instance Eq Surface where
  _ == _ = True
  -- we should only have one

--------------------------------------------------------------------------------
-- * SkCanvasRef

 -- | A reference to the SkCanvas
newtype SkCanvasRef = MkSkCanvasRef JSVal
  deriving newtype (JS.MakeObject, ToJSVal)

instance Show SkCanvasRef where
  show _ = "SkCanvasRef"
instance Eq SkCanvasRef where
  _ == _ = True
  -- we should only have one

--------------------------------------------------------------------------------

-- | Action to initailize the CanvasKit there is nohandler; i..e. your model should
-- somehow store this CanvasKit object and th surface.
data InitializeSkCanvasAction =
    InitializeSkCanvas CanvasKit Surface

--------------------------------------------------------------------------------

-- | We need to initialize the CanvasKit. This needs to fetch the CanvasKit wasm module,
-- so this is a subscription; i.e. we listen to when the CanvasKit module has been loaded.
initializeCanvasKitSub             :: MisoString -> Sub InitializeSkCanvasAction
initializeCanvasKitSub theCanvasId = \sink -> do
   -- withCKFunction is the callback; i.e. the thing that we do with the given CanvasKit
   -- value
   withCKFunction <- JS.function $ storeCanvasKitAndSurface theCanvasId sink
    --  We grab the ckLoaded value,
   ckLoaded <- jsg ("ckLoaded" :: MisoString)
   jsg ("console" :: MisoString) ^. JS.js1 ("log" :: MisoString) ckLoaded
   -- and call it with the '
   ckLoaded ^.js1 ("then" :: MisoString) withCKFunction
   pure ()


-- | Function to Store the canvasKit object
storeCanvasKitAndSurface                              :: MisoString
                                                      -> Sink InitializeSkCanvasAction
                                                      -> JS.JSCallAsFunction
storeCanvasKitAndSurface theCanvasId sink _fObj _this = \case
  [ckJSVal] -> do surfJSVal <- ckJSVal ^.js1 ("MakeCanvasSurface" :: MisoString) theCanvasId
                  let canvasKit = MkCanvasKit ckJSVal
                      surface'  = MkSurface surfJSVal
                  -- someshow store the canvasKit and the surface
                  liftIO $ sink $ InitializeSkCanvas canvasKit surface'
                  -- requestAnimationFrame canvasKit surface'
  _         -> pure () -- TODO this should probably be some error?

-- | Calls requestAnimationFrame
requestAnimationFrame                        :: CanvasKit
                                             -> Surface
                                             -> (CanvasKit -> SkCanvasRef -> JSM ())
                                             -- ^ the drawing function
                                             -> JSM ()
requestAnimationFrame canvasKit surface draw = do
    runDraw' <- JS.function draw'
    _        <- surface ^.js1 ("requestAnimationFrame" :: MisoString) runDraw'
    pure ()
  where
    draw'     :: JS.JSCallAsFunction
    draw' _ _ = \case
      [skCanvasRefJSVal] -> draw canvasKit (MkSkCanvasRef skCanvasRefJSVal)
      _                  -> pure () -- TODO: again, this should probably be an error
