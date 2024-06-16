{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE TemplateHaskell            #-}
module SkiaCanvas.Core
  (
  -- * Model
    Canvas
  , theViewport
  , blankCanvas
  , HasDimensions(..)

  , HasMousePosition(..)
  , mouseCoordinates

  , canvasKitRefs
  , surfaceRef

  -- * Controller
  , InternalCanvasAction(..)
  , handleInternalCanvasAction
  , handleCanvasKitAction

  , CanvasResizeAction(..)
  , handleCanvasResize

  , ErrorAction(..)

  , acquireCanvasSize

  , withCanvasEvents

  -- * View
  , skiaCanvas_
  ) where

import           Control.Lens
import qualified Data.Map as Map
import           GHCJS.Marshal (fromJSVal)
import           HGeometry.Miso.Subscription.MouseExtra
import           HGeometry.Miso.Svg.Canvas (HasMousePosition(..))
import           HGeometry.Miso.Svg.StaticCanvas (HasDimensions(..))
import           HGeometry.Point
import           HGeometry.Vector
import           HGeometry.Viewport
import qualified Language.Javascript.JSaddle.Object as JS
import           Miso (Attribute, View, Effect, noEff, onMouseLeave, canvas_, id_, getElementById, JSM)
import           Miso.String (MisoString)
import           SkiaCanvas.CanvasKit
import           SkiaCanvas.CanvasKit.Initialize
import           SkiaCanvas.CanvasKit.Render

--------------------------------------------------------------------------------

-- | A Skia Canvas
data CanvasF f r = Canvas {  _theViewport  :: !(Viewport r)
                                           -- ^ the viewport
                          , _dimensions    :: !(Vector 2 Int)
                                           -- ^ dimensions (width,height) in pixels, of the canvas
                          , _mousePosition :: Maybe (Point 2 Int)
                                           -- ^ the mouse position, in raw pixel coordinates
                          , _canvasKitRefs :: f (CanvasKitRefs ())
                                           -- ^ references to the canvaskit
                          }

deriving instance (Show (f (CanvasKitRefs ())), Show r) => Show (CanvasF f r)
deriving instance (Eq   (f (CanvasKitRefs ())), Eq r)   => Eq   (CanvasF f r)

type Canvas = CanvasF Maybe


-- | Crate a blank canvas, that has the origin in the bottom-left.
blankCanvas     :: (Num r)
                 => Int -> Int -> CanvasF Maybe r
blankCanvas w h = let v = Vector2 w h
                  in Canvas { _theViewport   = flipY (fromIntegral <$> v)
                            , _dimensions    = v
                            , _mousePosition = Nothing
                            , _canvasKitRefs = Nothing
                            }

-- | Lens to access the viewport
theViewport :: Lens (Canvas r) (Canvas s) (Viewport r) (Viewport s)
theViewport = lens _theViewport (\c vp -> c { _theViewport = vp })

-- | Lens to access the canvasKit
canvasKitRefs :: Lens (CanvasF f r)                  (CanvasF g r)
                      (f (CanvasKitRefs ())) (g (CanvasKitRefs ()))
canvasKitRefs = lens _canvasKitRefs (\c ckRefs -> c { _canvasKitRefs = ckRefs })

-- | Lens to access the Surface
surfaceRef :: Traversal (Canvas r) (Canvas r) SurfaceRef SurfaceRef
surfaceRef = canvasKitRefs._Just.theSurface

  -- lens _surfaceRef (\c surfRef -> c { _surfaceRef = surfRef })


instance HasDimensions (Canvas r) (Vector 2 Int) where
  dimensions = lens _dimensions (\c d -> c { _dimensions = d })
  {-# INLINE dimensions #-}

instance HasMousePosition (Canvas r) (Maybe (Point 2 Int)) where
  mousePosition = lens _mousePosition (\c m -> c { _mousePosition = m })
  {-# INLINE mousePosition #-}

-- | Getter to access the mouse coordinates (in terms of the coordinate system as used by
-- the canvas). Returns a Nothing if the mouse is not currently on/over the canvas.
mouseCoordinates :: Fractional r => Getter (Canvas r) (Maybe (Point 2 r))
mouseCoordinates = to $ \m -> toWorldIn' (m^.theViewport) <$> m^.mousePosition
  where
    toWorldIn' vp p = toWorldIn vp (p&coordinates %~ fromIntegral)



--------------------------------------------------------------------------------
-- * The Controller

-- | Actions that CanvasAction will handle itself.
data InternalCanvasAction = MouseEnter !(Point 2 Int)
                          | MouseMove  !(Point 2 Int)
                          | MouseLeave
                          | TouchStart !(Point 2 Int)
                          | TouchMove  !(Point 2 Int)
                          | TouchEnd
                          deriving (Show,Eq)

-- | Handles InternalCanvas Actions
handleInternalCanvasAction        :: Canvas r -> InternalCanvasAction -> Effect action (Canvas r)
handleInternalCanvasAction canvas = noEff . \case
  MouseEnter p             -> canvas&mousePosition ?~ p
  MouseMove  p             -> canvas&mousePosition ?~ p
  MouseLeave               -> canvas&mousePosition .~ Nothing
  TouchStart p             -> canvas&mousePosition ?~ p
  TouchMove p              -> canvas&mousePosition ?~ p
  TouchEnd                 -> canvas&mousePosition .~ Nothing


handleCanvasKitAction        :: Canvas r -> InitializeSkCanvasAction -> Effect action (Canvas r)
handleCanvasKitAction canvas = noEff . \case
  InitializeRefs refs -> canvas&canvasKitRefs  ?~ refs

newtype CanvasResizeAction = SetCanvasSize (Vector 2 Int )
  deriving (Show,Read,Eq)

handleCanvasResize        :: Canvas r -> CanvasResizeAction -> Effect action (Canvas r)
handleCanvasResize canvas = noEff . \case
  SetCanvasSize v -> canvas&dimensions    .~ v

-- | Possible error
newtype ErrorAction = ErrorAction MisoString
  deriving (Show,Read,Eq,Ord)

--------------------------------------------------------------------------------
-- * Initializing/Binding to CanvasKit

-- | Acquire the current canvas size.
--
acquireCanvasSize             :: MisoString -> JSM (Either ErrorAction CanvasResizeAction)
acquireCanvasSize theCanvasId = do
    theCanvasElem <- getElementById theCanvasId
    wVal <- theCanvasElem JS.! ("offsetWidth"  :: MisoString)
    hVal <- theCanvasElem JS.! ("offsetHeight" :: MisoString)
    (theCanvasElem JS.<# ("width"  :: MisoString)) wVal
    (theCanvasElem JS.<# ("height" :: MisoString)) hVal
    w <- fromJSVal wVal
    h <- fromJSVal hVal
    case Vector2 <$> w <*> h of
      Nothing -> pure . Left  $ ErrorAction "acquireCanvasSize, something went wrong"
      Just v  -> pure . Right $ SetCanvasSize v


--------------------------------------------------------------------------------
-- * The View

-- | Renders a Skia canvas
skiaCanvas_           :: MisoString
                      -> [Attribute action]
                      -> View (Either InternalCanvasAction action)
skiaCanvas_ theId ats = canvas_ ([ id_ theId
                                 , onMouseEnterAt $ Left . MouseEnter
                                 , onMouseMoveAt  $ Left . MouseMove
                                 , onMouseLeave   $ Left MouseLeave
                                 ] <> (fmap Right <$> ats))
                                []


--------------------------------------------------------------------------------
-- * Canvas events that we should listen to

-- | Events a canvas wants to listen to
withCanvasEvents :: Map.Map MisoString Bool -> Map.Map MisoString Bool
withCanvasEvents = Map.union $ Map.fromList
                   [ ("touchstart"  , False)
                   , ("touchmove"   , False)
                   , ("touchend"    , False)
                   , ("mouseleave"  , True)
                   , ("mousemove"   , False)
                   , ("contextmenu" , False)
                   ]
