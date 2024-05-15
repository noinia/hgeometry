{-# LANGUAGE OverloadedStrings      #-}
module SkiaCanvas.CanvasKit.Core
  ( CanvasKit(..)
  , Surface(..)
  , SkCanvasRef(..)

  , InitializeSkCanvasAction(..)
  , initializeCanvasKitSub

  , requestAnimationFrame


  , mkWhite

  , SkPaintRef
  , SkPathRef
  , SkInputColor

  , clear
  , clearWith
  , circle

  , Cmd(..)
  , withPath
  , withPathFromCmds
  , drawPath

  , withPaint
  , setAntiAlias
  , setStyle
  , setColor

  , mkPaintStyle
  , SkPaintStyle
  , Style(..)

  , ColorInt
  , mkColor4f
  , mkColor
  -- , mkColorInt
  ) where

import           Color(Color, ColorF(..), Alpha(..), fromAlpha)
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


--------------------------------------------------------------------------------
-- * The CanvasKit object

 -- ^ the CanvasKit object
newtype CanvasKit = MkCanvasKit { ckAsJSVal :: JSVal}
  deriving newtype (JS.MakeObject,ToJSVal)

instance Show CanvasKit where
  show _ = "CanvasKitObj"
instance Eq CanvasKit where
  _ == _ = True
  -- we should only have one

--------------------------------------------------------------------------------
-- * Surface

 -- ^ the Surface object
newtype Surface = MkSurface JSVal
  deriving newtype (JS.MakeObject, ToJSVal)

instance Show Surface where
  show _ = "SurfaceObj"
instance Eq Surface where
  _ == _ = True
  -- we should only have one

--------------------------------------------------------------------------------
-- * SkCanvasRef

 -- | A reference to the SkCanvas
newtype SkCanvasRef = MkSkCanvasRef JSVal
  deriving newtype (JS.MakeObject, ToJSVal)

instance Show SkCanvasRef where
  show _ = "SkCanvasRef"
instance Eq SkCanvasRef where
  _ == _ = True
  -- we should only have one

--------------------------------------------------------------------------------

-- | Action to initailize the CanvasKit there is nohandler; i..e. your model should
-- somehow store this CanvasKit object and th surface.
data InitializeSkCanvasAction = InitializeRefs {-# UNPACK #-} !CanvasKit
                                               {-# UNPACK #-} !Surface

--------------------------------------------------------------------------------

-- | We need to initialize the CanvasKit. This needs to fetch the CanvasKit wasm module,
-- so this is a subscription; i.e. we listen to when the CanvasKit module has been loaded.
initializeCanvasKitSub             :: MisoString -> Sub InitializeSkCanvasAction
initializeCanvasKitSub theCanvasId = \sink -> do
   -- withCKFunction is the callback; i.e. the thing that we do with the given CanvasKit
   -- value. In particular, we will store the CanvasKit object and surface so we can use it later.
   withCKFunction <- JS.function $ storeCanvasKitAndSurface theCanvasId sink
    --  We grab the ckLoaded varaible, which represents the CanvasKit wasm object
   ckLoaded <- jsg ("ckLoaded" :: MisoString)
   -- and call the "then" function with our given callback. the callback will run when
   -- the wasm module has been loaded.
   _       <- ckLoaded ^.js1 ("then" :: MisoString) withCKFunction
   pure ()

-- | Function to Store the canvasKit object. In addition, it will clear the
-- canvas/painting it all white as an initial draw
storeCanvasKitAndSurface                              :: MisoString
                                                      -> Sink InitializeSkCanvasAction
                                                      -> JS.JSCallAsFunction
storeCanvasKitAndSurface theCanvasId sink _fObj _this = \case
  [ckJSVal] -> do surfJSVal <- ckJSVal ^.js1 ("MakeCanvasSurface" :: MisoString) theCanvasId
                  let canvasKit = MkCanvasKit ckJSVal
                      surface'  = MkSurface surfJSVal
                  -- someshow store the canvasKit and the surface
                  liftIO . sink $ InitializeRefs canvasKit surface'
                  requestAnimationFrame canvasKit surface' clear
  _         -> pure () -- TODO this should probably be some error?

-- | Calls requestAnimationFrame
requestAnimationFrame                        :: CanvasKit
                                             -> Surface
                                             -> (CanvasKit -> SkCanvasRef -> JSM ())
                                             -- ^ the drawing function
                                             -> JSM ()
requestAnimationFrame canvasKit surface draw = do
    runDraw' <- JS.function draw'
    _        <- surface ^.js1 ("requestAnimationFrame" :: MisoString) runDraw'
    pure ()
  where
    draw'     :: JS.JSCallAsFunction
    draw' _ _ = \case
      [skCanvasRefJSVal] -> draw canvasKit (MkSkCanvasRef skCanvasRefJSVal)
      _                  -> pure () -- TODO: again, this should probably be an error



--------------------------------------------------------------------------------
-- * The drawing functions

-- | Class indicating that something is an SkInput Color
class ToJSVal color => SkInputColor_ color
  -- there is no implementation since all these things are just newtype's over JsVals
  -- anyway.

instance SkInputColor_ SkInputColor
instance SkInputColor_ SkColor

mkWhite           :: CanvasKit -> JSM SkInputColor
mkWhite canvasKit = SkInputColor <$> canvasKit JS.! ("WHITE" :: MisoString)
-- white = SkInputColor ("white" :: MisoString)


-- an InputColor is apparently either an MallocObj | Color | number[];
-- CanvasKit APIs accept normal arrays, typed arrays, or Malloc'd memory as colors.
-- Length 4.


-- black = SkInputColor "black"

-- | Clear the canvas with white
clear                  :: CanvasKit -> SkCanvasRef -> JSM ()
clear canvasKit canvas = do white <- mkWhite canvasKit
                            clearWith canvas white

-- | Clear with a given color
clearWith              :: SkCanvasRef -> SkInputColor -> JSM ()
clearWith canvas color = void $ canvas ^.js1 ("clear" :: MisoString) color


-- | A color, in Skia's setup
newtype SkColor = SkColor JSVal
  deriving (ToJSVal)


-- | An input color, in Skia's setup
newtype SkInputColor = SkInputColor JSVal
  deriving (ToJSVal)

-- | A reference to an SkPath
newtype SkPathRef = SkPathRef JSVal
  deriving (ToJSVal, JS.MakeObject)

-- | A reference to an SkPaint object
newtype SkPaintRef = SkPaintRef JSVal
  deriving (ToJSVal, JS.MakeObject)

-- | Apply some operations with a paint
withPaint           :: CanvasKit -> (SkPaintRef -> JSM a) -> JSM a
withPaint canvasKit =
    JSAddle.bracket (SkPaintRef <$> (JS.new (canvasKit JS.! ("Paint" :: MisoString)) ()))
                    (\paint -> paint ^. JS.js0 ("delete" :: MisoString))

--------------------------------------------------------------------------------
-- * Simple objects (Rectangles, Circles)

circle                :: ( Ball_ circle (Point 2 r)
                         , ToJSVal r
                         , Radical r
                         ) => SkCanvasRef -> circle -> SkPaintRef -> JSM ()
circle canvas c paint = do Point2 x y <- (c^.center)&coordinates %%~ toJSVal
                           r          <- toJSVal $ c^.radius
                           void $ canvas ^.js4 ("drawCircle" :: MisoString) x y r paint

--------------------------------------------------------------------------------
-- * Path's

-- | Apply some operations with a path reference.
withPath           :: CanvasKit -> (SkPathRef -> JSM a) -> JSM a
withPath canvasKit =
    JSAddle.bracket (SkPathRef <$> (JS.new (canvasKit JS.! ("Path" :: MisoString)) ()))
                    (\path -> path ^. JS.js0 ("delete" :: MisoString))

-- | Apply some operations with a path reference.
withPathFromCmds                :: (Traversable f, Real r)
                                => CanvasKit
                                -> f (Cmd r)
                                -> (SkPathRef -> JSM a) -> JSM a
withPathFromCmds canvasKit cmds =
    JSAddle.bracket (do cmds'    <- concat <$> traverse (fromCmd canvasKit) cmds
                        cmdArray <- toJSVal cmds'
                        pathObj  <- canvasKit JS.! ("Path" :: MisoString)
                        SkPathRef <$> (pathObj ^.js1 ("MakeFromCmds" :: MisoString) cmdArray)
                    )
                    (\path -> path ^. JS.js0 ("delete" :: MisoString))

data Cmd r = MoveTo  (Point 2 r)
           | LineTo  (Point 2 r)
           | QuadTo  (Point 2 r) (Point 2 r)
           | ConicTo (Point 2 r) (Point 2 r) r
           | CubicTo (Point 2 r) (Point 2 r) (Point 2 r)
           | Close
           deriving (Show,Eq)

instance Functor Cmd where
  fmap f = \case
    MoveTo p      -> MoveTo $ p&coordinates %~ f
    LineTo p      -> LineTo $ p&coordinates %~ f
    QuadTo p q    -> QuadTo (p&coordinates %~ f) (q&coordinates %~ f)
    ConicTo p q w -> ConicTo (p&coordinates %~ f) (q&coordinates %~ f) (f w)
    CubicTo p q r -> CubicTo (p&coordinates %~ f) (q&coordinates %~ f) (r&coordinates %~ f)
    Close         -> Close

fromCmd   ::  Real r => CanvasKit -> Cmd r -> JSM [JSVal]
fromCmd ck = fromFloatCmd ck . fmap realToFrac

fromFloatCmd   :: CanvasKit -> Cmd Float -> JSM [JSVal]
fromFloatCmd _ = \case
    MoveTo p      -> do Point2 x y <- p&coordinates %%~ toJSVal
                        moveTo <- toJSVal moveVerb
                        pure [moveTo, x , y]
    LineTo p      -> do Point2 x y <- p&coordinates %%~ toJSVal
                        lineTo <- toJSVal lineVerb
                        pure [lineTo, x , y]
    QuadTo p q    -> do Point2 px py <- p&coordinates %%~ toJSVal
                        Point2 qx qy <- q&coordinates %%~ toJSVal
                        quadTo <- toJSVal quadVerb
                        pure [quadTo, px , py, qx, qy]
    ConicTo p q w -> do Point2 px py <- p&coordinates %%~ toJSVal
                        Point2 qx qy <- q&coordinates %%~ toJSVal
                        w'           <- toJSVal w
                        conicTo      <- toJSVal conicVerb
                        pure [conicTo, px , py, qx, qy, w']
    CubicTo p q r -> do Point2 px py <- p&coordinates %%~ toJSVal
                        Point2 qx qy <- q&coordinates %%~ toJSVal
                        Point2 rx ry <- r&coordinates %%~ toJSVal
                        cubicTo <- toJSVal cubicVerb
                        pure [cubicTo, px , py, qx, qy, rx, ry]
    Close         -> do close' <- toJSVal closeVerb
                        pure [close']
  where
    moveVerb  = 0 :: Int -- CanvasKit.MOVE_VERB
    lineVerb  = 1 :: Int -- CanvasKit.LINE_VERB
    quadVerb  = 2 :: Int -- CanvasKit.QUAD_VERB
    conicVerb = 3 :: Int -- CanvasKit.CONIC_VERB
    cubicVerb = 4 :: Int -- CanvasKit.CUBIC_VERB
    closeVerb = 5 :: Int -- CanvasKit.CLOSE_VERB

-- |  call drawPath
drawPath                   :: SkCanvasRef -> SkPathRef -> SkPaintRef -> JSM ()
drawPath canvas path paint =
  void $ canvas ^.js2 ("drawPath" :: MisoString) path paint

--------------------------------------------------------------------------------

-- | Set antialias
setAntiAlias                :: SkPaintRef -> Bool -> JSM ()
setAntiAlias paint shouldAA = void $ paint ^.js1 ("setAntiAlias" :: MisoString) shouldAA

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
instance Apply JSM where
  (<.>) = (<*>)
  (<. ) = (<* )
  ( .>) = ( *>)
