{-# LANGUAGE OverloadedStrings          #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}
module Miso.Subscription.MouseExtra
  ( onMouseEnterAt
  , onMouseMoveAt
  , onMouseClickAt
  , onTouchStartAt
  , onTouchMoveAt
  , onTouchEnd
  ) where

import           Control.Monad.IO.Class
import           Data.Aeson (withObject, withArray, (.:), Value)
import           Data.Aeson.Types (Parser)
import qualified Data.Foldable as F
import           GHCJS.Marshal
import           HGeometry.Point
import           JavaScript.Object
import           JavaScript.Object.Internal
import           Language.Javascript.JSaddle (JSVal)
import           Miso
import           Miso.FFI.Extra
import           Miso.String (MisoString)

--------------------------------------------------------------------------------

-- | onMouseMove event, the position is relative to the target of the event
onMouseMoveAt :: (Point 2 Int -> action) -> Attribute action
onMouseMoveAt = on "mousemove" mousePositionDecoder

-- | onMouseEnter event, the position is relative to the target of the event
onMouseEnterAt :: (Point 2 Int -> action) -> Attribute action
onMouseEnterAt = on "mouseenter" mousePositionDecoder

-- | onMouseEnter event, the position is relative to the target of the event
onMouseClickAt :: (Point 2 Int -> action) -> Attribute action
onMouseClickAt = on "click" mousePositionDecoder

-- | Mouse position decoder that captures the position of the event relative to the
-- target. In particular, it reads the offsetX and offsetY values of the event.
mousePositionDecoder :: Decoder (Point 2 Int)
mousePositionDecoder = Decoder dec dt
  where
    dt = DecodeTarget mempty
    dec = withObject "event" $ \o -> Point2 <$> o .: "offsetX" <*> o .: "offsetY"

--------------------------------------------------------------------------------

-- | On start of a touch event,
onTouchStartAt :: (Point 2 Int -> action) -> Attribute action
onTouchStartAt = on "touchstart" touchDecoder

-- | On touchMove event
onTouchMoveAt :: (Point 2 Int -> action) -> Attribute action
onTouchMoveAt = on "touchmove" touchDecoder

-- | onTouchEnd event
onTouchEnd     :: action -> Attribute action
onTouchEnd act = on "touchend" emptyDecoder (const act)

-- | The decoder of touch events, gets the touchTarget of the input
touchDecoder :: Decoder (Point 2 Int)
touchDecoder = Decoder dec dt
  where
    dt = DecodeTarget ["targetTouches"]
    dec :: Value -> Parser (Point 2 Int)
    dec = withArray "targetTouches" $ \arr -> case F.toList arr of
      (tv:_) -> withObject "touch" (\o ->
                  Point2 <$> o .: "pageX"   <*> o .: "pageY") tv
      _      -> fail "touchDecoder: expected at least one targetTouches"
