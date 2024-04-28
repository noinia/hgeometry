{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
module SkiaCanvas
  ( Canvas
  , theViewport
  , blankCanvas
  , HasDimensions(..)

  , HasMousePosition(..)
  , mouseCoordinates

  , InternalCanvasAction
  , handleInternalCanvasAction
  , withCanvasEvents

  , skiaCanvas_
  ) where

import           Control.Lens
import qualified Data.Map as Map
import           HGeometry.Miso.Svg.Canvas (HasMousePosition(..))
import           HGeometry.Miso.Svg.StaticCanvas (HasDimensions(..))
import           HGeometry.Point
import           HGeometry.Vector
import           HGeometry.Viewport
import           Miso (Attribute, View, Effect, noEff, onMouseLeave, canvas_, id_)
import           Miso.String (MisoString)
import           MouseExtra

--------------------------------------------------------------------------------

-- | A Skia Canvas
data Canvas r = Canvas {  _theViewport  :: !(Viewport r)
                                        -- ^ the viewport
                       , _dimensions    :: !(Vector 2 Int)
                                        -- ^ dimensions (width,height) in pixels, of the canvas
                       , _mousePosition :: Maybe (Point 2 Int)
                                        -- ^ the mouse position, in raw pixel coordinates
                       }
              deriving (Show,Eq)

-- | Crate a blank canvas, that has the origin in the bottom-left.
blankCanvas     :: (Num r)
                 => Int -> Int -> Canvas r
blankCanvas w h = let v = Vector2 w h
                  in Canvas (flipY (fromIntegral <$> v)) v Nothing

-- | Lens to access the viewport
theViewport :: Lens (Canvas r) (Canvas s) (Viewport r) (Viewport s)
theViewport = lens _theViewport (\c vp -> c { _theViewport = vp })

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
  MouseEnter p  -> canvas&mousePosition ?~ p
  MouseMove  p  -> canvas&mousePosition ?~ p
  MouseLeave    -> canvas&mousePosition .~ Nothing
  TouchStart p  -> canvas&mousePosition ?~ p
  TouchMove p   -> canvas&mousePosition ?~ p
  TouchEnd      -> canvas&mousePosition .~ Nothing

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
-- * Canvas events that we should isten to

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
