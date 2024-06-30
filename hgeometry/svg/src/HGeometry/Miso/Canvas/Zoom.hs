module HGeometry.Miso.Canvas.Zoom
  ( ZoomDirection(..)
  , HasZoomLevel(..)
  , ZoomAction(..)
  , update
  ) where

import Control.Lens
import HGeometry.Interval
import Miso (Effect, noEff)


--------------------------------------------------------------------------------
-- * Model

data ZoomDirection = ZoomIn | ZoomOut deriving (Show,Read,Eq)

-- | Types that have a zoom-level
class HasZoomLevel canvas r | canvas -> r where
  -- | Lens to access the current zoom-level of the canvas
  zoomLevel :: Lens' canvas r

--------------------------------------------------------------------------------
-- * Controller

newtype ZoomAction = ZoomAction ZoomDirection deriving (Show,Eq)

-- | Update the zoom-level
update      :: ( Fractional r, Ord r
               , HasZoomLevel canvas r
               )
            => canvas -> ZoomAction -> Effect action canvas
update m za = m&zoomLevel %%~ flip updateZoom' za


updateZoom'   :: (Fractional r, Ord r)
             => r -> ZoomAction -> Effect action r
updateZoom' z = \case
    ZoomAction dir                -> noEff $ applyZoom dir z


applyZoom       :: forall r.(Fractional r, Ord r) => ZoomDirection -> r -> r
applyZoom dir z = let delta = case dir of
                                ZoomIn  -> 0.1
                                ZoomOut -> (-1)*0.1
                  in clampTo rng (z + delta)
  where
    rng = ClosedInterval @r 0.5 10
