{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE OverloadedStrings          #-}
module SkiaCanvas.CanvasKit.Initialize
  ( CanvasKitRefs(CanvasKitRefs)
  , theCanvasKit, theSurface, theCanvas, strokeOnly, fillOnly, pictureRecorder
  , HasCanvasKitRef(..)


  , InitializeSkCanvasAction(..)
  , initializeCanvasKitSub
  ) where


import           Control.Lens
import           Control.Monad.IO.Class
import           Language.Javascript.JSaddle (JSM)
import           Language.Javascript.JSaddle.Object (js1, jsg)
import qualified Language.Javascript.JSaddle.Object as JS
import           Miso
import           Miso.String (MisoString)
import           SkiaCanvas.CanvasKit.Core
import           SkiaCanvas.CanvasKit.Paint (SkPaintStyle, mkPaintStyle, Style(..))
import           SkiaCanvas.CanvasKit.PictureRecorder

--------------------------------------------------------------------------------

-- | References to CanvasKit objects we maintain
data CanvasKitRefs skCanvas = CanvasKitRefs
  { _theCanvasKit    :: {-# UNPACK #-}!CanvasKit
  , _theSurface      :: {-# UNPACK #-}!SurfaceRef
  , _theCanvas       :: skCanvas
  , _strokeOnly      :: {-# UNPACK #-}!SkPaintStyle
  , _fillOnly        :: {-# UNPACK #-}!SkPaintStyle
  , _pictureRecorder :: {-# UNPACK #-}!SkPictureRecorderRef
  }

makeLenses ''CanvasKitRefs

instance Show (CanvasKitRefs skCanvas) where
  show _ = "CanvasKitRefs"
instance Eq (CanvasKitRefs skCanvas) where
  _ == _ = True
  -- we should only have one


class HasCanvasKitRef t where
  canvasKitRef :: Lens' t CanvasKit

instance HasCanvasKitRef (CanvasKitRefs skCanvas) where
  canvasKitRef = theCanvasKit



-- | Action to initailize the CanvasKit there is nohandler; i..e. your model should
-- somehow store this CanvasKit object and th surface.
newtype InitializeSkCanvasAction = InitializeRefs (CanvasKitRefs ())

--------------------------------------------------------------------------------

-- | We need to initialize the CanvasKit. This needs to fetch the CanvasKit wasm module,
-- so this is a subscription; i.e. we listen to when the CanvasKit module has been loaded.
initializeCanvasKitSub             :: MisoString -> Sub InitializeSkCanvasAction
initializeCanvasKitSub theCanvasId = \sink -> do
   -- withCKFunction is the callback; i.e. the thing that we do with the given CanvasKit
   -- value. In particular, we will store the CanvasKit object and surface so we can use it later.
   withCKFunction <- JS.function $ storeCanvasKitAndSurface theCanvasId sink
    --  We grab the ckLoaded varaible, which represents the CanvasKit wasm object
   ckLoaded <- jsg ("ckLoaded" :: MisoString)
   -- and call the "then" function with our given callback. the callback will run when
   -- the wasm module has been loaded.
   _       <- ckLoaded ^.js1 ("then" :: MisoString) withCKFunction
   pure ()

-- | Function to Store the canvasKit object. In addition, it will clear the
-- canvas/painting it all white as an initial draw
storeCanvasKitAndSurface                              :: MisoString
                                                      -> Sink InitializeSkCanvasAction
                                                      -> JS.JSCallAsFunction
storeCanvasKitAndSurface theCanvasId sink _fObj _this = \case
  [ckJSVal] -> do surfJSVal <- ckJSVal ^.js1 ("MakeCanvasSurface" :: MisoString) theCanvasId
                  let canvasKit = MkCanvasKit ckJSVal
                      surface'  = MkSurfaceRef surfJSVal
                  strokeOnly' <- mkPaintStyle canvasKit StrokeOnly
                  fillOnly'   <- mkPaintStyle canvasKit FillOnly
                  picRec      <- createPictureRecorder canvasKit
                  let refs = CanvasKitRefs canvasKit surface' () strokeOnly' fillOnly' picRec
                  -- someshow store the refs
                  sink $ InitializeRefs refs
                  requestAnimationFrame canvasKit surface' clear
  _         -> pure () -- TODO this should probably be some error?
