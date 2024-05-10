{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
module CanvasKit
  ( CanvasKit
  , ckAsJSVal


  , Surface
  , SkCanvasRef


  , InitializeSkCanvasAction(..)
  , initializeCanvasKitSub

  , requestAnimationFrame


  , mkWhite

  , clear, clearWith
  , drawPoint
  , drawCircle


  , circle
  , withPath
  , withPaint
  ) where

import           Control.Lens
import           Control.Monad (void)
import           Control.Monad.IO.Class
import           Data.Functor.Apply (Apply(..))
import qualified Data.Map as Map
import           GHCJS.Marshal (fromJSVal, ToJSVal(..))
import           GHCJS.Types
import           HGeometry.Ball
import           HGeometry.Number.Radical (Radical)
import           HGeometry.Point
import           HGeometry.Vector
import           HGeometry.Viewport
-- import qualified JavaScript.Array as JSArray
import qualified Language.Javascript.JSaddle as JSAddle
import           Language.Javascript.JSaddle.Object (jsg1, jsg2, jsf, js1, js4, jsg)
import qualified Language.Javascript.JSaddle.Object as JS
import           Miso
import           Miso.String (MisoString)
import           MouseExtra

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
data InitializeSkCanvasAction =
    InitializeSkCanvas CanvasKit Surface

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
                  liftIO $ sink $ InitializeSkCanvas canvasKit surface'
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

mkWhite           :: CanvasKit -> JSM SkInputColor
mkWhite canvasKit = SkInputColor <$> canvasKit JS.! ("WHITE" :: MisoString)
-- white = SkInputColor ("white" :: MisoString)

-- black = SkInputColor "black"

-- | Clear the canvas with white
clear                  :: CanvasKit -> SkCanvasRef -> JSM ()
clear canvasKit canvas = do white <- mkWhite canvasKit
                            clearWith canvas white

-- | Clear with a given color
clearWith              :: SkCanvasRef -> SkInputColor -> JSM ()
clearWith canvas color = void $ canvas ^.js1 ("clear" :: MisoString) color

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


drawPoint          :: ( Point_ point 2 r
                      , HasCoordinates point (Point 2 Float)
                      , Real r
                      )
                   => SkCanvasRef -> point -> SkPaintRef -> JSM ()
drawPoint canvas p = let toFloat = realToFrac :: Real r => r -> Float
                     in circle canvas (Disk (p&coordinates %~ toFloat) 3)

drawCircle             :: forall circle point r.
                          ( Ball_ circle point
                          , Point_ point 2 r
                          , HasCoordinates point (Point 2 Float)
                          , Real r
                          ) => SkCanvasRef -> circle -> SkPaintRef -> JSM ()
drawCircle canvas c = let ctr = c^.center
                          c'  = Disk (ctr&coordinates %~ toFloat) (toFloat $ c^.squaredRadius)
                          toFloat = realToFrac :: r -> Float
                       in circle canvas c'

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
                        SkPathRef <$> (canvasKit ^.js1 ("MakeFromCmds" :: MisoString)
                                                       cmdArray)
                    )
                    (\path -> path ^. JS.js0 ("delete" :: MisoString))

data Cmd r = MoveTo (Point 2 r)
           | LineTo (Point 2 r)
           | QuadTo (Point 2 r) (Point 2 r)
           deriving (Show,Eq)

instance Functor Cmd where
  fmap f = \case
    MoveTo p   -> MoveTo $ p&coordinates %~ f
    LineTo p   -> LineTo $ p&coordinates %~ f
    QuadTo p q -> QuadTo (p&coordinates %~ f) (q&coordinates %~ f)

fromCmd   ::  Real r => CanvasKit -> Cmd r -> JSM [JSVal]
fromCmd ck = fromFloatCmd ck . fmap realToFrac

fromFloatCmd   :: CanvasKit -> Cmd Float -> JSM [JSVal]
fromFloatCmd _ = \case
    MoveTo p   -> do Point2 x y <- p&coordinates %%~ toJSVal
                     moveTo <- toJSVal moveVerb
                     pure [moveTo, x , y]
    LineTo p   -> do Point2 x y <- p&coordinates %%~ toJSVal
                     lineTo <- toJSVal lineVerb
                     pure [lineTo, x , y]
    QuadTo p q -> do Point2 px py <- p&coordinates %%~ toJSVal
                     Point2 qx qy <- q&coordinates %%~ toJSVal
                     quadTo <- toJSVal quadVerb
                     pure [quadTo, px , py, qx, qy]
  where
    moveVerb = 0 :: Int -- CanvasKit.MOVE_VERB
    lineVerb = 1 :: Int -- CanvasKit.LINE_VERB
    quadVerb = 2 :: Int -- CanvasKit.QUAD_VERB

--------------------------------------------------------------------------------

--
instance Apply JSM where
  (<.>) = (<*>)
  (<. ) = (<* )
  ( .>) = ( *>)
