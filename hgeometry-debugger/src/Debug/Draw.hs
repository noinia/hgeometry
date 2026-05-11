module Debug.Draw
  ( traceDrawId
  , traceDraw
  , traceDrawIO
  , clearLayer
  , clear



  --------------------------------------------------------------------------------
  , debugClient
  , clientDrawLayer

  --------------------------------------------------------------------------------
  , draw'
  ) where


import HGeometry.Miso.Svg
import Debugger.API
import Network.HTTP.Client (newManager, defaultManagerSettings)
import System.IO.Unsafe (unsafePerformIO)
import           Servant.Client ( ClientM, runClientM, ClientEnv, mkClientEnv
                                , BaseUrl(..), client, Scheme(..)
                                )
import Data.Text.Encoding (decodeUtf8Lenient)
import Miso (View, ms, text, toMisoString)
import Data.ByteString (ByteString, toStrict)
import Data.ByteString.Char8 (pack)
import Servant.API
import Data.Proxy
import Miso.Html.Render

--------------------------------------------------------------------------------

-- | Trace and draw
traceDrawId         :: (Show a, Drawable a) => LayerName -> a -> a
traceDrawId layer a = traceDraw layer a a

-- | Trace and draw an a on the given layer, while returning a b.
--
-- this will add to the current layer
traceDraw           :: (Show a, Drawable a) => LayerName -> a -> b -> b
traceDraw layer a b = unsafePerformIO $ traceDrawIO layer a b
{-# NOINLINE traceDraw #-}

-- | Implementation of traceDraw
traceDrawIO           :: (Show a,Drawable a) => LayerName -> a -> b -> IO b
traceDrawIO layer a b = b <$ debugClient (clientDrawLayer (layer,show a, draw' a))

-- | Clears a particular layer
clearLayer      :: LayerName -> IO ()
clearLayer layer = debugClient $ clientClearLayer layer

-- | Clears all layers
clear :: IO ()
clear = debugClient clientClear

--------------------------------------------------------------------------------

draw' :: Drawable a => a -> Drawing
draw' = Drawing . toMisoString . decodeUtf8Lenient . toStrict . toHtml . flip draw []

--------------------------------------------------------------------------------
-- * The client

clientDrawing
  :<|> clientDrawLayer
  :<|> clientClearLayer
  :<|> clientClear
  = client (Proxy @API)

-- type Client = ReaderT ClientEnv IO

-- | Run some client action; just print the error if it fails somehow.
debugClient     :: ClientM a -> IO ()
debugClient act = runClientM act debugClientEnv >>= \case
                    Left err -> putStrLn $ "error: " <> show err
                    Right _  -> pure ()

-- | The default debug clientEnv
debugClientEnv :: ClientEnv
debugClientEnv = unsafePerformIO debugClientEnv'
{-# NOINLINE debugClientEnv #-}

-- | Creates a default ClientEnv
debugClientEnv' :: IO ClientEnv
debugClientEnv' = do mgr <- newManager defaultManagerSettings
                     pure $ mkClientEnv mgr defaultBaseUrl

defaultBaseUrl :: BaseUrl
defaultBaseUrl = BaseUrl Http defaultHost defaultPort ""
