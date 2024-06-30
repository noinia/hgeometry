{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE OverloadedStrings      #-}
module SkiaCanvas.CanvasKit.Core
  ( CanvasKit(..)
  , SurfaceRef(..)

  , SkCanvas_
  , SkCanvasRef(..)

  , SkInputColor

  , requestAnimationFrame

  , clear
  , clearWith
  , mkWhite
  ) where

import           Control.Lens
import           Control.Monad (void)
import           Data.Functor.Apply (Apply(..))
import           GHCJS.Marshal (ToJSVal(..))
import           GHCJS.Types
import qualified Language.Javascript.JSaddle as JSAddle
import           Language.Javascript.JSaddle.Object (js1)
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
-- * SurfaceRef

 -- ^ the SurfaceRef object
newtype SurfaceRef = MkSurfaceRef JSVal
  deriving newtype (JS.MakeObject, ToJSVal)

instance Show SurfaceRef where
  show _ = "SurfaceObj"
instance Eq SurfaceRef where
  _ == _ = True
  -- we should only have one

--------------------------------------------------------------------------------
-- * SkCanvasRef

-- | Types that can act as a SkCanvas
class (ToJSVal skCanvas, JS.MakeObject skCanvas) => SkCanvas_ skCanvas

 -- | A reference to the SkCanvas
newtype SkCanvasRef = MkSkCanvasRef JSVal
  deriving newtype (JS.MakeObject, ToJSVal)

instance Show SkCanvasRef where
  show _ = "SkCanvasRef"
instance Eq SkCanvasRef where
  _ == _ = True
  -- we should only have one

instance SkCanvas_ SkCanvasRef

--------------------------------------------------------------------------------

-- | Calls requestAnimationFrame
requestAnimationFrame                        :: CanvasKit
                                             -> SurfaceRef
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
clear                  :: SkCanvas_ skCanvas => CanvasKit -> skCanvas -> JSM ()
clear canvasKit canvas = do white <- mkWhite canvasKit
                            clearWith canvas white

-- | Clear with a given color
clearWith              :: SkCanvas_ skCanvas => skCanvas -> SkInputColor -> JSM ()
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
