{-# LANGUAGE OverloadedStrings      #-}
module SkiaCanvas.CanvasKit.PictureRecorder
  ( SkPictureRecorderRef(..)
  , withPictureRecorder
  , createPictureRecorder

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
  JSAddle.bracket (createPictureRecorder canvasKit)
                  (\pr -> pr ^. JS.js0 ("delete" :: MisoString))

-- | Create a new picturerecorder.
createPictureRecorder           :: CanvasKit -> JSM SkPictureRecorderRef
createPictureRecorder canvasKit =
  SkPictureRecorderRef <$> (JS.new (canvasKit JS.! ("PictureRecorder" :: MisoString)) ())
  -- TODO: this thing should be deleted at some point, which we currently don't


-- | Record the drawing commands
recordAsPicture :: SkInputRect_ skInputRect
                => CanvasKit
                -> SkPictureRecorderRef
                -> skInputRect -- ^ the initial bounds in which to draw
                -> (SkCanvasRef -> JSM ()) -- ^ the drawing commands
                -> JSM SkPictureRef
recordAsPicture canvasKit pr bounds draw =
  do canvasRef <- beginPictureRecorder canvasKit pr bounds FixedBounds --ComputeBounds
     draw canvasRef
     consoleLog ("done drawing" :: MisoString)
     finishRecordingAsPicture canvasKit pr


-- consoleLog arg = ("console" :: MisoString) JS.js1 ("log" :: MisoString) arg

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
