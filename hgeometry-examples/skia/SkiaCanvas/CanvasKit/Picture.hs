{-# LANGUAGE OverloadedStrings      #-}
module SkiaCanvas.CanvasKit.Picture
  ( SkPictureRef(..)
  ) where

import           GHCJS.Marshal (ToJSVal(..))
import           GHCJS.Types
import qualified Language.Javascript.JSaddle as JSAddle
import qualified Language.Javascript.JSaddle.Object as JS
import           SkiaCanvas.CanvasKit.Core

--------------------------------------------------------------------------------

-- | A reference to a SkPicture object
newtype SkPictureRef = SkPictureRef JSVal
  deriving (ToJSVal, JS.MakeObject)

-- we can use a SkPicture as a SkCanvas
instance SkCanvas_ SkPictureRef
