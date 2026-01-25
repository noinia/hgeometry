{-# LANGUAGE OverloadedStrings          #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Miso.Event.Extra
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Some extra event helpers, in partricular for touch and wheel events
--
--------------------------------------------------------------------------------
module HGeometry.Miso.Event.Extra
  ( WheelDirection(..)
  , onWheel
  , Button(..)
  , onClickWithButton
  , onRightClick
  -- , Touch(..), TouchEvent(..)
  -- , onTouchStart
  -- , onTouchMove
  -- , onTouchEnd
  ) where

import Data.Map qualified as Map
import Miso.JSON
import Miso
import Miso.Html.Event
import Miso.Event.Types as Event
import Miso.Util.Parser ()

--------------------------------------------------------------------------------

-- | Scroll-wheel direction
data WheelDirection = Up | Down deriving (Show,Eq)

-- | A type modelling the mouse buttons
data Button = LeftButton
            | MiddleButton
            | RightButton
            deriving (Show,Eq)

-- | on wheel events
onWheel          :: (WheelDirection -> action) -> Attribute action
onWheel toAction = on "wheel" (Decoder dec dt) (\res _ -> toAction res)
  where
    dt = DecodeTarget mempty
    dec = withObject "event" $ \o -> f <$> (o .: "deltaY")
    f   :: Double -> WheelDirection
    f x = if x < 0 then Up else Down

-- | get the mouse button that was clicked
onClickWithButton          :: (Button -> action) -> Attribute action
onClickWithButton toAction = on "click" (Decoder dec dt) (\res _ -> toAction res)
  where
    dt  = DecodeTarget mempty
    dec :: Value -> Parser Button
    dec = withObject "event" $ \o -> case Map.lookup "button" o of
            Nothing -> pfail "button not found?"
            Just v  -> flip (withNumber "Button") v $ \case
              0 -> pure LeftButton
              1 -> pure MiddleButton
              2 -> pure RightButton
              _ -> pfail "unknown button"

-- | Get right clicks
onRightClick :: action -> Attribute action
onRightClick = onContextMenuWithOptions disabled
  where
    disabled = Event.defaultOptions { _preventDefault  = True
                                    , _stopPropagation = False
                                    }


pfail   :: a -> b
pfail _ = error "miso does not yet expose this :S" -- FIXME: for now
