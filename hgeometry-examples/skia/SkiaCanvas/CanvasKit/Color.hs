{-# LANGUAGE OverloadedStrings      #-}
module SkiaCanvas.CanvasKit.Color
  ( SkInputColor_
  , SkInputColor(..)
  , SkColor(..)
  , ColorInt
  , mkColor4f
  , mkColor
  ) where

import           Color (Color, ColorF(..), Alpha(..), fromAlpha)
import           Control.Lens
import           Control.Monad (void)
import           Control.Monad.IO.Class
import           Data.Colour.SRGB
import           Data.Functor.Apply (Apply(..))
import           Data.Word (Word8)
import           GHCJS.Marshal (ToJSVal(..))
import           GHCJS.Types
import           HGeometry.Ball
import           HGeometry.Number.Radical (Radical)
import           HGeometry.Point
import qualified Language.Javascript.JSaddle as JSAddle
import           Language.Javascript.JSaddle.Object (js1, js2, js4, jsg)
import qualified Language.Javascript.JSaddle.Object as JS
import           Miso
import           Miso.String (MisoString)
import           SkiaCanvas.CanvasKit.Core

--------------------------------------------------------------------------------

-- | A color, in Skia's setup
newtype SkColor = SkColor JSVal
  deriving (ToJSVal)


type ColorInt = (RGB Word8, Alpha Float)

mkColor4f                                     :: CanvasKit -> Color -> JSM SkColor
mkColor4f canvasKit (Color c alpha) = let RGB r g b = toSRGB c
                                          a         = fromAlpha 1 alpha
                                      in
  SkColor <$> canvasKit ^.js4 ("Color4f" :: MisoString) r g b a

mkColor                              :: CanvasKit -> ColorInt -> JSM SkColor
mkColor canvasKit (RGB r g b, alpha) = let a = fromAlpha 1 alpha in
  SkColor <$> canvasKit ^.js4 ("Color" :: MisoString) r g b a

mkColorInt                              :: CanvasKit -> ColorInt -> JSM SkColor
mkColorInt canvasKit (RGB r g b, alpha) = let a = fromAlpha 1 alpha in
  SkColor <$> canvasKit ^.js4 ("ColorAsInt" :: MisoString) r g b a



-- | Class indicating that something is an SkInput Color
class ToJSVal color => SkInputColor_ color
  -- there is no implementation since all these things are just newtype's over JsVals
  -- anyway.

instance SkInputColor_ SkInputColor
instance SkInputColor_ SkColor

-- white = SkInputColor ("white" :: MisoString)


-- an InputColor is apparently either an MallocObj | Color | number[];
-- CanvasKit APIs accept normal arrays, typed arrays, or Malloc'd memory as colors.
-- Length 4.


-- black = SkInputColor "black"
