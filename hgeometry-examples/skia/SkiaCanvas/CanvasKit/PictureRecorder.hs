{-# LANGUAGE OverloadedStrings      #-}
module SkiaCanvas.CanvasKit.PictureRecorder
  ( SkPictureRecorderRef(..)
  , withPictureRecorder

  , recordAsPicture
  , beginPictureRecorder
  , finishRecordingAsPicture

  , ComputeBounds(..)
  ) where

import           Control.Lens
import           GHCJS.Marshal (ToJSVal(..))
import           GHCJS.Types
import qualified Language.Javascript.JSaddle as JSAddle
import           Language.Javascript.JSaddle.Object (js0, js2)
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

-- | Record the drawing commands
recordAsPicture :: SkInputRect_ skInputRect
                => CanvasKit
                -> SkPictureRecorderRef
                -> skInputRect -- ^ the initial bounds in which to draw
                -> (SkCanvasRef -> JSM ()) -- ^ the drawing commands
                -> JSM SkPictureRef
recordAsPicture canvasKit pr bounds draw =
  do canvasRef <- beginPictureRecorder canvasKit pr bounds ComputeBounds
     draw canvasRef
     finishRecordingAsPicture canvasKit pr


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
