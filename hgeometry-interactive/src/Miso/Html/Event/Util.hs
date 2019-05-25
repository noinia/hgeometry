{-# LANGUAGE OverloadedStrings #-}
module Miso.Html.Event.Util where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson.Types (parseEither)
import           Data.JSString
import qualified Data.Map as M
import           Data.Proxy
import           Data.String (IsString(..))
import qualified Data.Text as T
import           GHCJS.Marshal
import           GHCJS.Types
import qualified JavaScript.Array as JSArray
import           JavaScript.Object
import           JavaScript.Object.Internal (Object (Object))

import           Miso
import           Miso.Effect (Sink)
import           Miso.Effect (Sub)
import           Miso.Event.Decoder
import           Miso.Event.Types
import           Miso.String

-- | @onWithOptions opts eventName decoder toAction@ is an attribute
-- that will set the event handler of the associated DOM node to a function that
-- decodes its argument using @decoder@, converts it to an action
-- using @toAction@ and then feeds that action back to the @update@ function.
--
-- @opts@ can be used to disable further event propagation.
--
-- > let clickHandler = onWithOptions defaultOptions "click" emptyDecoder $ \() -> Action
-- > in button_ [ clickHandler, class_ "add" ] [ text_ "+" ]
--
onWithOptions
  :: Options
  -> MisoString
  -> Decoder r
  -> (r -> action)
  -> Attribute action
onWithOptions options eventName (Decoder dec dat) toAction =
  Attribute $ \sink n -> do
   eventObj <- getProp "events" n
   eventHandlerObject@(Object eo) <- create
   jsOptions <- toJSVal options
   decodeAtVal <- toJSVal dat
   cb <- callbackToJSVal <=< asyncCallback1 $ \e -> do
       Just v <- fromJSVal =<< objectToJSON decodeAtVal e
       case parseEither dec v of
         Left s -> error $ "Parse error on " <> unpack eventName <> ": " <> s
         Right r -> liftIO (sink (toAction r))
   set "runEvent" cb eventHandlerObject
   registerCallback cb
   set "options" jsOptions eventHandlerObject
   set eventName eo (Object eventObj)
