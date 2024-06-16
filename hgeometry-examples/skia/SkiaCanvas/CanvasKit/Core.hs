{-# LANGUAGE OverloadedStrings      #-}
module SkiaCanvas.CanvasKit.Core
  ( CanvasKit(..)
  , Surface(..)
  , SkCanvasRef(..)

  , SkInputColor

  , InitializeSkCanvasAction(..)
  , initializeCanvasKitSub

  , requestAnimationFrame


  , clear
  , clearWith
  , mkWhite
  ) where

import           Control.Lens
import           Control.Monad (void)
import           Control.Monad.IO.Class
import           Data.Functor.Apply (Apply(..))
import           GHCJS.Marshal (ToJSVal(..))
import           GHCJS.Types
import qualified Language.Javascript.JSaddle as JSAddle
import           Language.Javascript.JSaddle.Object (js1, jsg)
import qualified Language.Javascript.JSaddle.Object as JS
import           Miso
import           Miso.String (MisoString)


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
data InitializeSkCanvasAction = InitializeRefs {-# UNPACK #-} !CanvasKit
                                               {-# UNPACK #-} !Surface

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
                      surface'  = MkSurface surfJSVal
                  -- someshow store the canvasKit and the surface
                  liftIO . sink $ InitializeRefs canvasKit surface'
                  requestAnimationFrame canvasKit surface' clear
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



--------------------------------------------------------------------------------

-- | An input color, in Skia's setup
newtype SkInputColor = SkInputColor JSVal
  deriving (ToJSVal)



-- | Clear the canvas with white
clear                  :: CanvasKit -> SkCanvasRef -> JSM ()
clear canvasKit canvas = do white <- mkWhite canvasKit
                            clearWith canvas white

-- | Clear with a given color
clearWith              :: SkCanvasRef -> SkInputColor -> JSM ()
clearWith canvas color = void $ canvas ^.js1 ("clear" :: MisoString) color


mkWhite           :: CanvasKit -> JSM SkInputColor
mkWhite canvasKit = SkInputColor <$> canvasKit JS.! ("WHITE" :: MisoString)



--------------------------------------------------------------------------------

--------------------------------------------------------------------------------



-- data SkPaint = SkPaint { _antiAlias  :: {-# UNPACK #-}!Bool
--                        , _color      :: Color
--                        , _style      :: {-# UNPACK #-}!Style
--                        , _strokeWith :: {-# UNPACK #-}!Int -- float?
--                        }
--                deriving (Show,Eq,Ord)

 -- paint2.setAntiAlias(true);
 --    paint2.setColor(SkColorSetRGB(0, 136, 0));
 --    paint2.setStyle(SkPaint::kStroke_Style);
 --    paint2.setStrokeWidth(SkIntToScalar(3));


--------------------------------------------------------------------------------

--
instance Apply JSM where
  (<.>) = (<*>)
  (<. ) = (<* )
  ( .>) = ( *>)
