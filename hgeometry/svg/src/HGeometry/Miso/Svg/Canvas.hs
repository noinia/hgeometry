{-# LANGUAGE OverloadedStrings          #-}
module HGeometry.Miso.Svg.Canvas
  ( Canvas
  , theViewport
  , blankCanvas
  , HasDimensions(..)

  , HasMousePosition(..)
  , mouseCoordinates

  , InternalCanvasAction
  , handleInternalCanvasAction
  , subs

  , svgCanvas_
  ) where

import Control.Lens hiding (elements)
import HGeometry.Miso.Svg.StaticCanvas
import HGeometry.Point
import HGeometry.Transformation
import HGeometry.Vector
import HGeometry.Viewport
import Miso (Attribute, View, Effect, Sub, height_, width_, noEff, onMouseLeave)
import Miso.String (MisoString, ms)
import Miso.Subscription.MouseExtra
import Miso.Svg (svg_, g_, transform_)

--------------------------------------------------------------------------------
-- *A Canvas

-- | Svg Canvas that has a "proper" Coordinate system whose origin is in the bottom left.
data Canvas r =
  Canvas { _theViewport :: !(Viewport r)
         -- ^ the viewport
         , _dimensions  :: !(Vector 2 Int)
         -- ^ dimensions (width,height) in pixels, of the canvas
         , _mousePosition     :: Maybe (Point 2 Int)
         -- ^ the mouse position, in raw pixel coordinates
         }
  deriving stock (Eq)

-- | Lens to access the viewport
theViewport :: Lens (Canvas r) (Canvas s) (Viewport r) (Viewport s)
theViewport = lens _theViewport (\c vp -> c { _theViewport = vp })

instance HasDimensions (Canvas r) (Vector 2 Int) where
  dimensions = lens _dimensions (\c d -> c { _dimensions = d })
  {-# INLINE dimensions #-}

-- | Class for types that have a mouse position
class HasMousePosition s a | s -> a where
  -- | Lens to access the raw mouse position
  mousePosition :: Lens' s a

instance HasMousePosition (Canvas r) (Maybe (Point 2 Int)) where
  mousePosition = lens _mousePosition (\c m -> c { _mousePosition = m })
  {-# INLINE mousePosition #-}

-- | Getter to access the mouse coordinates (in terms of the coordinate system as used by
-- the canvas). Returns a Nothing if the mouse is not currently on/over the canvas.
mouseCoordinates :: Fractional r => Getter (Canvas r) (Maybe (Point 2 r))
mouseCoordinates = to $ \m -> toWorldIn' (m^.theViewport) <$> m^.mousePosition
  where
    toWorldIn' vp p = toWorldIn vp (p&coordinates %~ fromIntegral)

-- center     :: Lens' (Canvas r) (Point 2 r)
-- center     = lens _center     (\cv c -> cv { _center     = c } )

-- zoomLevel  :: Lens' (Canvas r) r
-- zoomLevel  = lens _zoomLevel      (\cv c -> cv { _zoomLevel      = c } )

--------------------------------------------------------------------------------

-- | Crate a blank canvas, that has the origin in the bottom-left.
blankCanvas     :: (Num r)
                 => Int -> Int -> Canvas r
blankCanvas w h = let v = Vector2 w h
                  in Canvas (flipY (fromIntegral <$> v)) v Nothing

--------------------------------------------------------------------------------
-- * The Controller

-- | Actions that CanvasAction will handle itself.
data InternalCanvasAction = MouseMove (Maybe (Point 2 Int))
                          | MouseLeave
                          deriving (Show,Eq)

-- | Handles InternalCanvas Actions
handleInternalCanvasAction        :: Canvas r -> InternalCanvasAction -> Effect action (Canvas r)
handleInternalCanvasAction canvas = noEff . \case
  MouseMove mp -> canvas&mousePosition .~ mp
  MouseLeave   -> canvas&mousePosition .~ Nothing


--------------------------------------------------------------------------------
-- * The View

-- | Draws the actual canvas
svgCanvas_               :: (RealFrac r, ToSvgCoordinate r)
                         => Canvas r
                         -> [Attribute action] -> [View action]
                         -> View (Either InternalCanvasAction action)
svgCanvas_ canvas ats vs =
  svg_ ([ width_   . ms $ w
        , height_  . ms $ h
        , onMouseLeave $ Left MouseLeave
        ] <> (fmap Right <$> ats))
        [ g_ [ transform_ ts ] (fmap Right <$> vs)
        ]
  where
    (Vector2 w h) = canvas^.dimensions
    ts = matrixToMisoString $ canvas^.theViewport.worldToHost.transformationMatrix


-- | To be used instead of the text_ combinator in Miso
-- textAt                     :: ToSvgCoordinate r
--                            => Point 2 r -- ^ position where to draw (in world coordinates)
--                            -> [Attribute action]
--                            -> MisoString -> View action
-- textAt (Point2 x y) ats t = g_ [ transform_ $ mconcat [ "translate("
--                                                        , ms x
--                                                        , ", "
--                                                        , ms y
--                                                        , ")scale(1,-1)"
--                                                        ]
--                                 ] [ Miso.text_ ats [text t] ]



-- class RenderWebSvg t where
--   renderWith :: t -> [Attribute action] -> [View action] -> View action

-- render       :: RenderWebSvg t => t -> [Attribute action] -> View action
-- render x ats = renderWith x ats []

-- instance ToSvgCoordinate r => RenderWebSvg (Point 2 r) where
--   renderWith (Point2 x y) ats = ellipse_ $ [ cx_ $ ms x, cy_ $ ms y
--                                            , rx_ "5", ry_ "5"
--                                            , fill_ "black"
--                                            ] <> ats

--------------------------------------------------------------------------------
-- * Subscriptions

-- | Subscription needed for the iCanvas. In particular, captures the
-- mouse position
subs     :: MisoString -- ^ The id of the iCanvas
                -> (InternalCanvasAction -> action)
                -> [Sub action]
subs i f = [ relativeMouseSub i (f . MouseMove)
                  -- , arrowsSub          (f . ArrowPress)
                 ]
