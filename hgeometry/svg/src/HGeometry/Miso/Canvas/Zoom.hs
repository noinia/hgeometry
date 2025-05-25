module HGeometry.Miso.Canvas.Zoom
  ( ZoomDirection(..)
  , HasZoomLevel(..)
  , ZoomAction(..)
  , update
  ) where

import Control.Lens
import Control.Monad.State
import HGeometry.Interval
import Miso (Effect, put)


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


type instance Zoomed (EffectCore model action) = Zoomed

(Lazy.RWST r w s z) = FocusingWith w z
deriving newtype instance Zoom (EffectCore model action) (EffectCore a action)



-- | Update the zoom-level
update      :: ( Fractional r, Ord r
               , HasZoomLevel canvas r
               )
            => ZoomAction -> Effect canvas action
update za = zoom zoomLevel $ updateZoom' za

  -- updateZoom' za

  -- zoomLevel <%= updateZoom' za

  -- do zl <- gets (^.zoomLevel)

  --              zl' <- updateZoom' za


  -- zoom zoomLevel $ updateZoom' za

  -- do m <- get

  -- modify $ \m -> m&zoomLevel %%~ updateZoom' za

  -- zoom zoomLevel (updateZoom' za)

  -- updateZoom' aza (m^.zoomLevel)

  -- m&zoomLevel %%~ flip updateZoom' za


updateZoom' :: (Fractional r, Ord r)
            => ZoomAction -> Effect r action
updateZoom' = \case
    ZoomAction dir  -> modify $ applyZoom dir


applyZoom       :: forall r.(Fractional r, Ord r) => ZoomDirection -> r -> r
applyZoom dir z = let delta = case dir of
                                ZoomIn  -> 0.1
                                ZoomOut -> (-1)*0.1
                  in clampTo rng (z + delta)
  where
    rng = ClosedInterval @r 0.5 10
