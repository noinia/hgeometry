{-# LANGUAGE OverloadedStrings      #-}
module SkiaCanvas.CanvasKit.Picture
  ( SkPictureRef(..)
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

-- | A reference to a SkPicture object
newtype SkPictureRef = SkPictureRef JSVal
  deriving (ToJSVal, JS.MakeObject)

-- we can use a SkPicture as a SkCanvas
instance SkCanvas_ SkPictureRef
