{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE OverloadedStrings      #-}
module SkiaCanvas.CanvasKit.Image
  ( SkImageRef
  , withImageSnapshot
  , pattern FullScreen
  , Quality(..)

  , EncodedImageFormat(..)
  , encodeToBytes
  ) where

import           Control.Lens
import           Control.Monad (void)
import qualified Data.ByteString.Lazy as Lazy
import           Data.Coerce
import           Data.Default
import           Data.Functor.Apply (Apply(..))
import           Data.Word (Word8)
import           GHCJS.Marshal (ToJSVal(..))
import           GHCJS.Types
import           JavaScript.TypedArray.Internal.Types ( SomeTypedArray(..)
                                                      , IOUint8Array
                                                      , Uint8Array
                                                      )
import           Language.Javascript.JSaddle (JSM)
import qualified Language.Javascript.JSaddle as JSAddle
import           Language.Javascript.JSaddle.Object (js0, js1, js2, jsg)
import qualified Language.Javascript.JSaddle.Object as JS
import           Miso
import           Miso.String (MisoString)
import           SkiaCanvas.CanvasKit.Core
import           SkiaCanvas.CanvasKit.GeomPrims (SkInputIRectRef)
import           SkiaCanvas.CanvasKit.Picture (uint8ArrayToByteString)

--------------------------------------------------------------------------------

 -- ^ the SkImageRef object
newtype SkImageRef = MkSkImageRef JSVal
  deriving newtype (JS.MakeObject, ToJSVal)

-- | Helper pattern
pattern FullScreen :: Maybe SkInputIRectRef
pattern FullScreen = Nothing


-- | make an snapshot of the current surface
withImageSnapshot  :: SurfaceRef
                   -> Maybe SkInputIRectRef -- ^ dimensions; Nothing means full screen
                   -> (SkImageRef -> JSM a)
                   -> JSM a
withImageSnapshot surface dims = JSAddle.bracket (makeImageSnapshot surface dims)
                                                 (\img -> img ^. js0 ("delete" :: MisoString))


-- | make an snapshot of the current surface
makeImageSnapshot         :: SurfaceRef
                          -> Maybe SkInputIRectRef -- ^ dimensions; Nothing means full screen
                          -> JSM SkImageRef
makeImageSnapshot surface = \case
  Nothing   -> MkSkImageRef <$> surface ^.js0 ("makeImageSnapshot" :: MisoString)
  Just dims -> MkSkImageRef <$> surface ^.js1 ("makeImageSnapshot" :: MisoString) (toJSVal dims)


data EncodedImageFormat = Png | Jpeg | Webp
                        deriving (Show,Eq,Ord)

instance Default EncodedImageFormat where
  def = Png


-- | Reference to the encoded image format
newtype SkEncodedImageFormatRef = SkEncodedImageFormatRef JSVal
  deriving (ToJSVal, JS.MakeObject)


-- | Look up the reference to the encodedImageFormat
getEncodedImageFormat               :: CanvasKit -> EncodedImageFormat
                                    -> JSM SkEncodedImageFormatRef
getEncodedImageFormat canvasKit fmt =
    SkEncodedImageFormatRef <$> canvasKit JS.! ("ImageFormat" :: MisoString) JS.! fmtStr
  where
    fmtStr :: MisoString = case fmt of
      Png  -> "PNG"
      Jpeg -> "JPEG"
      Webp -> "WEBP"

-- | From 0 to 100, 100 is the best
newtype Quality = Quality Word8
  deriving (Show,Eq,Ord,Enum, ToJSVal)

instance Default Quality where
  def = maxBound

instance Bounded Quality where
  minBound = Quality 0
  maxBound = Quality 100

-- | Encode the image as a lazy bytestring
encodeToBytes                   :: CanvasKit
                                -> SkImageRef
                                -> EncodedImageFormat -> Quality -> JSM Lazy.ByteString
encodeToBytes canvasKit image fmt quality = do
  fmtRef <- getEncodedImageFormat canvasKit fmt
  consoleLogX image
  someArray' <- image ^.js2 ("encodeToBytes" :: MisoString) fmtRef quality
  consoleLogX someArray'
  let someArray = SomeTypedArray someArray'
  uint8ArrayToByteString $ coerce someArray

consoleLogX   :: ToJSVal a => a -> JSM ()
consoleLogX x = void $ jsg ("console" :: MisoString) ^.js1 ("log" :: MisoString) x


--------------------------------------------------------------------------------
