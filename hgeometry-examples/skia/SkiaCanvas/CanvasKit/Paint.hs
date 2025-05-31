{-# LANGUAGE OverloadedStrings      #-}
module SkiaCanvas.CanvasKit.Paint
  ( SkPaintRef
  , withPaint
  , setAntiAlias
  , setStyle
  , setColor

  , mkPaintStyle
  , SkPaintStyle
  , Style(..)
  ) where

import           Control.Lens
import           Control.Monad (void)
import           GHCJS.Marshal (ToJSVal(..))
import           GHCJS.Types
import           Language.Javascript.JSaddle (JSM)
import qualified Language.Javascript.JSaddle as JSAddle
import           Language.Javascript.JSaddle.Object (js1)
import qualified Language.Javascript.JSaddle.Object as JS
import           Miso
import           Miso.String (MisoString)
import           SkiaCanvas.CanvasKit.Color
import           SkiaCanvas.CanvasKit.Core

--------------------------------------------------------------------------------

-- | A reference to an SkPaint object
newtype SkPaintRef = SkPaintRef JSVal
  deriving (ToJSVal, JS.MakeObject)


-- | Apply some operations with a paint
withPaint           :: CanvasKit -> (SkPaintRef -> JSM a) -> JSM a
withPaint canvasKit =
    JSAddle.bracket (SkPaintRef <$> (JS.new (canvasKit JS.! ("Paint" :: MisoString)) ()))
                    (\paint -> paint ^. JS.js0 ("delete" :: MisoString))



-- | Set antialias
setAntiAlias                :: SkPaintRef -> Bool -> JSM ()
setAntiAlias paint shouldAA = void $ paint ^.js1 ("setAntiAlias" :: MisoString) shouldAA

--------------------------------------------------------------------------------


data Style = FillOnly | StrokeOnly
  deriving (Show,Read,Eq,Ord,Enum)

-- | Get the paintStyle
mkPaintStyle             :: CanvasKit -> Style -> JSM SkPaintStyle
mkPaintStyle canvasKit s = do
                             paintStyle <- canvasKit JS.! ("PaintStyle" :: MisoString)
                             SkPaintStyle <$> paintStyle JS.! symb
  where
    symb = case s of
             FillOnly   -> "Fill"   :: MisoString
             StrokeOnly -> "Stroke"

-- | A Paint style
newtype SkPaintStyle = SkPaintStyle JSVal
  deriving (ToJSVal)


setStyle         :: SkPaintRef -> SkPaintStyle -> JSM ()
setStyle paint s = void $ paint ^.js1 ("setStyle" :: MisoString) s


setColor         :: SkInputColor_ skInputColor => SkPaintRef -> skInputColor -> JSM ()
setColor paint c = void $ paint ^.js1 ("setColor" :: MisoString) c

-- setColorInt         :: SkInputColor_ skInputColor => SkPaintRef -> skInputColor -> JSM ()
-- setColorInt paint c = void $ paint ^.js1 ("setColorInt" :: MisoString) c


--------------------------------------------------------------------------------
