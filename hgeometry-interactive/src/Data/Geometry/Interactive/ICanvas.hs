{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
module Data.Geometry.Interactive.ICanvas( module Data.Geometry.Interactive.StaticCanvas
                                        , ICanvas(ICanvas), blankCanvas
                                        , canvas, mousePosition, panStatus
                                        , mouseCoordinates

                                        , CanvasAction(..)
                                        , update
                                        , view
                                        ) where

import           Control.Lens hiding (view, element, rmap, Zoom)
import           Data.Aeson.Types
import           Data.Geometry.Box
import           Data.Geometry.Interactive.StaticCanvas
import           Data.Geometry.Point
import           Data.Geometry.Vector
import           Data.Intersection (coRec)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import           Data.Proxy
import           Data.Range
import           Miso hiding (update, view)

import           Debug.Trace
--------------------------------------------------------------------------------
-- * Model

data ZoomDirection = ZoomIn | ZoomOut deriving (Show,Read,Eq)

data PanStatus r = NoPan
                 | PanFrom (Point 2 Int) -- ^ point from which we are panning
                           (Point 2 r) -- ^ center point of the viewport at that time
                 deriving (Show,Eq)

data ICanvas r = ICanvas { _canvas           :: Canvas r
                         , _mousePosition    :: Maybe (Point 2 Int)
                         , _panStatus        :: PanStatus r
                           -- ^ point where we started panning from
                         } deriving (Show,Eq)
makeLenses ''ICanvas


mouseCoordinates :: Fractional r => Getter (ICanvas r) (Maybe (Point 2 r))
mouseCoordinates = to $ \m -> realWorldCoordinates (m^.canvas) <$> m^.mousePosition


-- | Createas an interactive lbank canvas
blankCanvas     :: Num r => Int -> Int -> ICanvas r
blankCanvas w h = ICanvas (createCanvas w h) Nothing NoPan


--------------------------------------------------------------------------------
-- * Controller

data Capabilities = Zoomable
                  | Pannable
                  deriving (Show,Eq)




data CanvasAction = MouseMove (Int,Int)
                  | MouseLeave
                  | ArrowPress Arrows
                  | Pan PanAction
                  | Zoom ZoomAction
                  deriving (Show,Eq)



update   :: (Fractional r, Ord r) => ICanvas r -> CanvasAction -> Effect action (ICanvas r)
update m = \case
    MouseMove (x,y)         -> let p  = Point2 x y
                               in noEff $ m&mousePosition .~ Just p
                                           &canvas.center %~ applyPan (m^.panStatus) p
    MouseLeave              -> noEff $ m&mousePosition .~ Nothing
    ArrowPress (Arrows x y) -> let v   = ((*2) . fromIntegral) <$> Vector2 x y
                               in noEff $ m&canvas.center %~ (.+^ v)
    Pan pa                  -> updatePan m pa
    Zoom za                 -> updateZoom m za


----------------------------------------
-- ** Zooming

newtype ZoomAction = ZoomAction ZoomDirection deriving (Show,Eq)

updateZoom      :: (Fractional r, Ord r)
                => ICanvas r -> ZoomAction -> Effect action (ICanvas r)
updateZoom m za = m&canvas.zoomLevel %%~ flip updateZoom' za

updateZoom'   :: (Fractional r, Ord r)
             => r -> ZoomAction -> Effect action r
updateZoom' z = \case
    ZoomAction dir                -> noEff $ applyZoom dir z


applyZoom       :: (Fractional r, Ord r) => ZoomDirection -> r -> r
applyZoom dir z = let delta = case dir of
                                ZoomIn  -> 0.1
                                ZoomOut -> (-1)*0.1
                  in clampTo rng (z + delta)
  where
    rng = ClosedRange 0.5 10

----------------------------------------
-- ** Panning

data PanAction = StartPan
               | StopPan
               deriving (Show,Eq)

updatePan      :: Num r => ICanvas r -> PanAction -> Effect action (ICanvas r)
updatePan m pa = m&panStatus %%~ \_ -> updatePan' (m^.mousePosition) (m^.canvas.center) pa

updatePan'        :: Num r
                  => Maybe (Point 2 Int) -- ^ current mouse position
                  -> Point 2 r           -- ^ current center of the viewport
                  -> PanAction
                  -> Effect action (PanStatus r)
updatePan' mp c = noEff . \case
    StartPan                -> case mp of
                                 Nothing -> NoPan
                                 Just p  -> PanFrom p c
    StopPan                 -> NoPan


applyPan         :: Num r
                 => PanStatus r
                 -> Point 2 Int -- ^ current mouse position
                 -> Point 2 r   -- ^ current center
                 -> Point 2 r
applyPan ps p c = case ps of
  NoPan        -> c
  PanFrom q oc -> let Vector2 vx vy = fromIntegral <$> (p .-. q) in oc .+^  Vector2 ((-1)*vx) vy


--------------------------------------------------------------------------------
-- * View

view            :: ( RealFrac r, ToSvgCoordinate r)
                => (CanvasAction -> action)
                -> ICanvas r
                -> [Attribute action] -> [View action] -> View action
view f m ats vs = staticCanvas_ (m^.canvas)
                                ([ onMouseLeave $ f MouseLeave
                                 , onMouseDown  $ f (Pan StartPan)
                                 , onMouseUp    $ f (Pan StopPan)
                                 , onWheel      $ f . Zoom . ZoomAction
                                 ] <> ats) vs

onWheel :: (ZoomDirection -> action) -> Attribute action
onWheel = on "wheel" (Decoder dec dt)
  where
    dt = DecodeTarget mempty
    dec = withObject "event" $ \o -> (f <$> (o .: "deltaY"))
    f   :: Double -> ZoomDirection
    f x = if x < 0 then ZoomIn else ZoomOut
