{-# LANGUAGE OverloadedStrings      #-}
module SkiaCanvas.CanvasKit.PictureRecorder
  ( SkPictureRecorderRef(..)
  , withPictureRecorder
  , beginPictureRecorder
  , finishRecordingAsPicture
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
import           HGeometry.Box
import           HGeometry.Number.Radical (Radical)
import           HGeometry.Point
import qualified Language.Javascript.JSaddle as JSAddle
import           Language.Javascript.JSaddle.Object (js0, js1, js2, js4, jsg)
import qualified Language.Javascript.JSaddle.Object as JS
import           Miso
import           Miso.String (MisoString)
import           SkiaCanvas.CanvasKit.Core
import           SkiaCanvas.CanvasKit.GeomPrims
import           SkiaCanvas.CanvasKit.Picture

--------------------------------------------------------------------------------

-- | A reference to a PictureRecorder object
newtype SkPictureRecorderRef = SkPictureRecorderRef JSVal
  deriving (ToJSVal, JS.MakeObject)


-- | Run some computation with a picture recorder.
withPictureRecorder           :: CanvasKit -> (SkPictureRecorderRef -> JSM a) -> JSM a
withPictureRecorder canvasKit =
  JSAddle.bracket (SkPictureRecorderRef <$> canvasKit JS.! ("PictureRecorder" :: MisoString))
                  (\pr -> pr ^. JS.js0 ("delete" :: MisoString))

data ComputeBounds = FixedBounds | ComputeBounds
                   deriving (Show,Read,Eq,Ord)

beginPictureRecorder              :: SkInputRect_ skInputRect
                                  => CanvasKit
                                  -> SkPictureRecorderRef
                                  -> skInputRect
                                  -> ComputeBounds
                                  -> JSM SkCanvasRef
beginPictureRecorder canvasKit pr bounds computeBounds =
  MkSkCanvasRef <$> pr ^.js2 ("beginRecording" :: MisoString) bounds computeBounds'
  where
    computeBounds' = case computeBounds of
      FixedBounds   -> False
      ComputeBounds -> True

finishRecordingAsPicture              :: CanvasKit
                                      -> SkPictureRecorderRef
                                      -> JSM SkPictureRef
finishRecordingAsPicture canvasKit pr =
  SkPictureRef <$> pr ^.js0 ("finishRecordingAsPicture" :: MisoString)




-- asPicture :: CanvasKit -> SkPictureRecorderRef -> JSM () ->
