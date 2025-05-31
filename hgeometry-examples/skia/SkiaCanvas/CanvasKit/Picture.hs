{-# LANGUAGE OverloadedStrings      #-}
module SkiaCanvas.CanvasKit.Picture
  ( SkPictureRef(SkPictureRef)
  , drawPicture

  , serialize
  , withPicture

  , serialize'
  , withPicture'

  , uint8ArrayToByteString
  ) where

import           Control.Lens
import           Control.Monad (void)
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as Lazy
import           Data.Coerce
import qualified GHC.IsList as Lazy
import           GHCJS.Marshal (ToJSVal(..))
import           GHCJS.Types
import qualified JavaScript.TypedArray as TypedArray
import           JavaScript.TypedArray.Internal.Types ( SomeTypedArray(..)
                                                      , IOUint8Array
                                                      , Uint8Array
                                                      )
import           Language.Javascript.JSaddle (JSM, ghcjsPure)
import qualified Language.Javascript.JSaddle as JSAddle
import           Language.Javascript.JSaddle.Object (js0, js1)
import qualified Language.Javascript.JSaddle.Object as JS
import           Miso hiding (go)
import           Miso.String (MisoString)
import           SkiaCanvas.CanvasKit.Core

--------------------------------------------------------------------------------

-- | A reference to a SkPicture object
newtype SkPictureRef = SkPictureRef JSVal
  deriving (ToJSVal, JS.MakeObject)


instance Show SkPictureRef where
  show _ = "SkPictureRef"
instance Eq SkPictureRef where
  _ == _ = True
  -- FIXME: hack

-- we can use a SkPicture as a SkCanvas
instance SkCanvas_ SkPictureRef





-- | Renders a picture
drawPicture            :: SkCanvas_ skCanvas => skCanvas -> SkPictureRef -> JSM ()
drawPicture canvas pic = void $ canvas ^.js1 ("drawPicture" :: MisoString) pic

-- log'     :: ToJSVal arg => arg -> JSM ()
-- log' arg = void $ do
--   console <- JS.jsg ("console" :: MisoString)
--   console ^.JS.js1 ("log" :: MisoString) arg

--------------------------------------------------------------------------------
-- | Serialize a picture to a bytestring representing the 'skp' value
serialize      :: SkPictureRef -> JSM Lazy.LazyByteString
serialize pic = serialize' pic >>= uint8ArrayToByteString . coerce


-- | Converts an IOUint8Array into a lazy bytestring
uint8ArrayToByteString     :: IOUint8Array -> JSM Lazy.LazyByteString
uint8ArrayToByteString arr = do n <- ghcjsPure $ TypedArray.length arr
                                Builder.toLazyByteString <$> go0 n 0
  where
    go0 n = go
      where
        go i | i == n    = pure mempty
             | otherwise = ( \x bs -> Builder.word8 x <> bs
                         ) <$> TypedArray.unsafeIndex i arr
                           <*> go (succ i)

-- | Serialize a picture into a javascript Uint8Array
serialize'     :: SkPictureRef -> JSM Uint8Array
serialize' pic = SomeTypedArray <$> (pic ^. js0 ("serialize" :: MisoString))

--------------------------------------------------------------------------------

-- | Deserialize the bytestring
withPicture                :: CanvasKit
                           -> Lazy.ByteString
                           -> (SkPictureRef -> JSM a)
                           -> JSM a
withPicture canvasKit bs f = do
    (arr :: IOUint8Array) <- TypedArray.create (fromIntegral $ Lazy.length bs)
    iforM_ (Lazy.toList bs) $ \i x ->
      TypedArray.unsafeSetIndex i x arr
    withPicture' canvasKit (coerce arr) f

-- | Given the typed array representing the picture, use it
withPicture'                                :: CanvasKit
                                            -> Uint8Array
                                            -> (SkPictureRef -> JSM a)
                                            -> JSM a
withPicture' canvasKit (SomeTypedArray arr) =
  JSAddle.bracket (SkPictureRef <$> (canvasKit ^.js1 ("MakePicture" :: MisoString) arr))
                  (\pic -> pic ^. js0 ("delete" :: MisoString))
