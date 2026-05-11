module Main(main) where

import           Network.HTTP.Client (newManager, defaultManagerSettings)
import           Data.Tuple (swap)
import           Control.Monad.Reader
import qualified Data.Sequence as Seq
import           Data.Sequence (Seq(..))
import           Control.Monad.State.Class
import           Data.IORef
import           Miso (View, ms, text, toMisoString)
import           Data.ByteString (ByteString, toStrict)
import           Data.ByteString.Char8 (pack)
import           Miso.Html.Element (div_)
import           Miso.Html.Render
import           Servant
import           Network.Wai
import           Network.Wai.Handler.Warp
import qualified Data.Map as Map
import           Data.Map (Map)
import           Data.Aeson
import           Data.Text (Text)
import           GHC.Generics
import           Data.Kind (Type)
import           Control.Concurrent.Async
import           Network.HTTP.Media ((//), (/:))
import           Debugger.API
import           HGeometry.Point
import           HGeometry.Miso.Svg
import           Debug.Draw

--------------------------------------------------------------------------------


-- type Svg =


--------------------------------------------------------------------------------
-- * Our API


-- | Start the debug server, then do stuff
runDebugServer          :: Port -> IO () -> IO ()
runDebugServer port act = do putStrLn $ "Starting the server on port " <> show port
                             stateRef <- newIORef mempty
                             withAsync (run port $ app stateRef) $ \serverProcess -> do
                               act
                               wait serverProcess
                                 -- this will just wait indefinitely but whatever

--------------------------------------------------------------------------------
-- * IO Implementations of the tracing functions






--------------------------------------------------------------------------------


  -- "traceDraw" :>


--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
data Svg = Svg


data HTMLMiso

instance Accept HTMLMiso where
    contentType _ = pack "text" // pack "html" /: (pack "charset", pack "utf-8")

instance ToHtml a => MimeRender HTMLMiso a where
    mimeRender _ = toHtml

instance MimeRender HTMLMiso (View model action) where
    mimeRender _ = toHtml

--------------------------------------------------------------------------------


-- dummy
-- serverState = State $ Map.fromList [("dummy",Drawing)]

type StateRef = IORef Drawings


type Server' api = ServerT api Handler'


newtype Handler' a = Handler' {unHandler' :: ReaderT StateRef IO a}
                   deriving (Functor, Applicative, Monad, MonadReader StateRef, MonadIO)

--------------------------------------------------------------------------------

instance MonadState Drawings Handler' where
  state f = do ref <- ask
               liftIO $ atomicModifyIORef ref (swap . f)


server :: Server' ServerAPI
server =   serveDirectoryWebApp "pub"
      :<|> handleDrawing
      :<|> handleDrawLayer
      :<|> handleClearLayer
      :<|> handleClear

handleDrawing :: Handler' Drawings
handleDrawing = get

handleDrawLayer                             :: (LayerName, String, Drawing) -> Handler' ()
handleDrawLayer (layerName,content,drawing) =
  modify $ Map.insertWith (<>) layerName (Seq.singleton (content,drawing))

handleClearLayer       :: LayerName -> Handler' ()
handleClearLayer layer = modify $ Map.delete layer

handleClear :: Handler' ()
handleClear = put mempty

  -- do stateRef <- newIORef $ State mempty
  --           pure "foo"

app          :: StateRef -> Application
app stateRef = serve api $ hoistServer api (liftIO . flip runReaderT stateRef . unHandler') server
  where
    api = Proxy @ServerAPI

main :: IO ()
main = runDebugServer defaultPort $ do
  print "woei"
  debugClient $ clientDrawLayer ("myLayer","my layer content", draw' (Point2 50 (100 :: Int)))
  x <- read <$> getLine
  print $ fib x

fib :: Int -> Int
fib = \case
  0 -> 0
  1 -> 1
  x -> fib (x-1) + fib (x-2)
