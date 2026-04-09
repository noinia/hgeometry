{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE OverloadedStrings      #-}
module SkiaCanvas.CanvasKit.Core
  ( CanvasKit(..)
  , SurfaceRef(..)
  , flush

  , SkCanvas_
  , SkCanvasRef(..)

  , SkInputColor

  , requestAnimationFrame

  , clear
  , clearWith
  , mkWhite
  , toDataURL
  , Codec(..)
  , Base64EncodedString(..)



  ) where

import Control.Lens
import Control.Monad (void)
import Data.Functor.Apply (Apply(..))
import Data.Text (Text)
import Data.Coerce
import Miso hiding (requestAnimationFrame, flush)
import Miso.String (MisoString)
import Miso.FFI hiding (flush)

--------------------------------------------------------------------------------
-- * The CanvasKit object

 -- ^ the CanvasKit object
newtype CanvasKit = MkCanvasKit { ckAsJSVal :: JSVal}
  deriving newtype (ToJSVal, ToObject)

instance Show CanvasKit where
  show _ = "CanvasKitObj"
instance Eq CanvasKit where
  _ == _ = True
  -- we should only have one

--------------------------------------------------------------------------------
-- * SurfaceRef

 -- ^ the SurfaceRef object
newtype SurfaceRef = MkSurfaceRef JSVal
  deriving newtype (ToJSVal, ToObject)

instance Show SurfaceRef where
  show _ = "SurfaceObj"
instance Eq SurfaceRef where
  _ == _ = True
  -- we should only have one

-- | Flush drawing to the survace
flush         :: SurfaceRef -> IO ()
flush surface = void $ callFunction (coerce surface) ("flush" :: MisoString) ()

--------------------------------------------------------------------------------
-- * SkCanvasRef

-- | Types that can act as a SkCanvas
class ( ToJSVal skCanvas, ToObject skCanvas
      , Coercible skCanvas JSVal
      ) => SkCanvas_ skCanvas

 -- | A reference to the SkCanvas
newtype SkCanvasRef = MkSkCanvasRef JSVal
  deriving newtype (ToJSVal, ToObject, ToArgs)

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
                                             -> (CanvasKit -> SkCanvasRef -> IO ())
                                             -- ^ the drawing function
                                             -> IO ()
requestAnimationFrame canvasKit surface draw = do
    runDraw' <- syncCallback1 $ draw (coerce canvasKit) . MkSkCanvasRef
    void $ callFunction (coerce surface) ("requestAnimationFrame" :: MisoString) runDraw'

--------------------------------------------------------------------------------

-- | An input color, in Skia's setup
newtype SkInputColor = SkInputColor JSVal
  deriving newtype (ToJSVal,ToArgs)



-- | Clear the canvas with white
clear                  :: SkCanvas_ skCanvas => CanvasKit -> skCanvas -> IO ()
clear canvasKit canvas = do white <- mkWhite canvasKit
                            clearWith canvas white

-- | Clear with a given color
clearWith              :: SkCanvas_ skCanvas => skCanvas -> SkInputColor -> IO ()
clearWith canvas color = void $ callFunction (coerce canvas) "clear" color

mkWhite           :: CanvasKit -> IO SkInputColor
mkWhite canvasKit = SkInputColor <$> getProperty (coerce canvasKit) "WHITE"


-- | Save the canvas using the given codec to a base64 encoded string.
toDataURL              :: SkCanvas_ skCanvas => skCanvas -> Codec -> IO Base64EncodedString
toDataURL canvas codec = do
    jsValStr <- callFunction (coerce canvas) ("toDataURL" :: MisoString)
                                             (codecStr :: MisoString)
    fromJSValUnchecked jsValStr
  where
    codecStr = case codec of
                 PngCodec -> "image/png"
                 JpgCodec -> "image/jpg"

-- | Codec for storing DataURLs
data Codec = PngCodec | JpgCodec
  deriving (Show,Read,Eq,Ord)

--------------------------------------------------------------------------------

-- | Base64 encoded strings
newtype Base64EncodedString = Base64EncodedString MisoString
  deriving (Show,Eq,Ord)
  deriving newtype (ToJSVal, FromJSVal)


-- instance JSAddle.FromJSVal Base64EncodedString where
--   fromJSVal jsv =
--     fmap (\(JSAddle.JSString t) -> Base64EncodedString t) <$> JSAddle.fromJSVal jsv

-- writeToFile :: Base64EncodedString -> OsPath -> IO ()
-- writeToFile (Base64EncodedString t) =  undefined


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
