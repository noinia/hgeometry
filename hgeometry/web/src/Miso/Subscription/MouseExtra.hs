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
import           HGeometry.Vector
import           JavaScript.Object
import           JavaScript.Object.Internal
import           Language.Javascript.JSaddle (JSVal)
import           Miso
import           Miso.FFI.Extra
import           Miso.String (MisoString)

import Debug.Trace
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


touchDecoder :: Decoder (Point 2 Int)
touchDecoder = Decoder dec dt
  where
    dt = DecodeTarget ["targetTouches"]
    dec :: Value -> Parser (Point 2 Int)
    dec = withArray "targetTouches" $ \arr -> case F.toList arr of
      (tv:_) -> flip (withObject "touch") tv $ \t ->
                  Point2 <$> t .: "pageX"   <*> t .: "pageY"
      _      -> fail "touchDecoder: expected at least one targetTouches"


--------------------------------------------------------------------------------

-- | A DOMRect
data DOMRect = DOMRect { top    :: {-# UNPACK #-} !Int
                       , left   :: {-# UNPACK #-} !Int
                       , width  :: {-# UNPACK #-} !Int
                       , height :: {-# UNPACK #-} !Int
                       } deriving (Show,Eq)

getBoundingRect       :: JSVal -> JSM DOMRect
getBoundingRect elem' = do
    rect  <- Object <$> getBoundingClientRect elem'
    Just l <- fromJSVal =<< getProp "left"    rect
    Just t <- fromJSVal =<< getProp "top"     rect
    Just w <- fromJSVal =<< getProp "width"   rect
    Just h <- fromJSVal =<< getProp "height"  rect
    pure $ DOMRect l t w h

-- | Get the inner rectangle of an element (i.e. without its border) relative to the
-- viewport.
getInnerRect       :: JSVal -> JSM DOMRect
getInnerRect elem' = do
  Just cl <- fromJSVal =<< getProp "clientLeft"   (Object elem')
  Just ct <- fromJSVal =<< getProp "clientTop"    (Object elem')
  Just cr <- fromJSVal =<< getProp "clientRight"  (Object elem')
  Just cb <- fromJSVal =<< getProp "clientBottom" (Object elem')
  DOMRect l t w h <- getBoundingRect elem'
  pure $ DOMRect (l-cl) (t-ct) (w - cr) (h - cb)


--------------------------------------------------------------------------------


-- touchDecoder :: Decoder (Point 2 Int)
-- touchDecoder = Decoder dec dt
--   where
--     dt = DecodeTarget ["targetTouches"]
--     dec :: Value -> Parser (Point 2 Int)
--     dec = withArray "targetTouches" $ \arr -> case F.toList arr of
--       (tv:_) -> flip (withObject "touch") tv $ \t -> do
--                   clientXY <- Point2 <$> t .: "clientX"   <*> t .: "clientY"
--                   target   <- traceShow arr $ t .: "target"
--                   clientLR <- clientCoords target
--                   pure $ clientXY .-^ clientLR

--       _      -> fail "touchDecoder: expected at least one targetTouches"

--     clientCoords = withObject "target-client" $ \o ->
--                      Vector2 <$> o .: "clientLeft"   <*> o .: "clientTop"


-- -- | The decoder of touch events, gets the touchTarget of the input
-- touchDecoder :: Decoder (Point 2 Int)
-- touchDecoder = Decoder dec dt
--   where
--     dt = DecodeTarget mempty
--     dec = withObject "event" $ \e -> traceShow e $
--                                      do --target        <- e .:" target"
--                                         -- Vector2 cl ct <- clientCoords target
--                                         tts           <- e .: "targetTouches"
--                                         f tts

--     f :: Value -> Parser (Point 2 Int)
--     f = withArray "targetTouches-array" $ \arr -> case F.toList arr of
--       (tv:_) -> flip (withObject "touch") tv $ \o ->
--                   Point2 <$> o .: "clientX"   <*> o .: "clientY"
--       _      -> fail "touchDecoder: expected at least one targetTouches"


    -- clientCoords = withObject "target-client" $ \o ->
    --                  Vector2 <$> o .: "clientLeft"   <*> o .: "clientTop"



-- onWithOptions
--   :: Options
--   -> MisoString
--   -> Decoder r
--   -> (r -> action)
--   -> Attribute action
-- onWithOptions options eventName Decoder{..} toAction =
--   E $ \sink n -> do
--    eventObj <- getProp "events" n
--    eventHandlerObject@(Object eo) <- create
--    jsOptions <- toJSVal options
--    decodeAtVal <- toJSVal decodeAt
--    cb <- callbackToJSVal <=< asyncCallback1 $ \e -> do
--        Just v <- fromJSVal =<< objectToJSON decodeAtVal e
--        case parseEither decoder v of
--          Left s -> error $ "Parse error on " <> unpack eventName <> ": " <> s
--          Right r -> liftIO (sink (toAction r))
--    set "runEvent" cb eventHandlerObject
--    registerCallback cb
--    set "options" jsOptions eventHandlerObject
--    set eventName eo (Object eventObj)
