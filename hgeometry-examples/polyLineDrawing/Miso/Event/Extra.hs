{-# LANGUAGE OverloadedStrings          #-}
module Miso.Event.Extra
  ( WheelDirection(..)
  , onWheel
  , Button(..)
  , onClickWithButton
  , onRightClick

  , onContextMenu
  ) where

import qualified Data.Aeson.KeyMap as Aeson
import Data.Aeson.Types
import Miso
import qualified Miso.Html.Event as Event

--------------------------------------------------------------------------------

data WheelDirection = Up | Down deriving (Show,Eq)


data Button = LeftButton
            | MiddleButton
            | RightButton
            deriving (Show,Eq)

-- | on wheel events
onWheel :: (WheelDirection -> action) -> Attribute action
onWheel = on "wheel" (Decoder dec dt)
  where
    dt = DecodeTarget mempty
    dec = withObject "event" $ \o -> (f <$> (o .: "deltaY"))
    f   :: Double -> WheelDirection
    f x = if x < 0 then Up else Down

-- | get the mouse button that was clicked
onClickWithButton :: (Button -> action) -> Attribute action
onClickWithButton = on "click" (Decoder dec dt)
  where
    dt  = DecodeTarget mempty
    dec :: Value -> Parser Button
    dec = withObject "event" $ \o -> case Aeson.lookup "button" o of
            Nothing -> fail "button not found?"
            Just v  -> flip (withScientific "Button") v $ \case
              0 -> pure LeftButton
              1 -> pure MiddleButton
              2 -> pure RightButton
              _ -> fail "unknown button"

-- | Get right clicks
onRightClick :: action -> Attribute action
onRightClick = onContextMenu


-- | prevent onContextMenu events
onContextMenu     :: action -> Attribute action
onContextMenu act = onWithOptions disabled "contextmenu" emptyDecoder (const act)
  where
    disabled = Event.defaultOptions { preventDefault  = True
                                    , stopPropagation = False
                                    }
