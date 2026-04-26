module Main(main) where

import           Network.HTTP.Client (newManager, defaultManagerSettings)
import           Data.Tuple (swap)
import           Control.Monad.Reader
import qualified Data.Sequence as Seq
import           Data.Sequence (Seq(..))
import           Control.Monad.State.Class
import           Data.IORef
import           Miso (View)
import           Servant
import           Network.Wai
import           Network.Wai.Handler.Warp
import qualified Data.Map as Map
import           Data.Map (Map)
import           Data.Aeson
import           GHC.Generics
import           System.IO.Unsafe (unsafePerformIO)
import           Data.Kind (Type)
import           Servant.Client
import           Control.Concurrent.Async

--------------------------------------------------------------------------------

type LayerName = String

-- type Svg =

class Draw t where
  -- | Draw something as svg
  draw :: t -> View model action

instance Draw Int where
  draw = undefined

draw' _ = Drawing

--------------------------------------------------------------------------------
-- * Our API

-- | Trace and draw
traceDrawId         :: (Show a, Draw a) => LayerName -> a -> a
traceDrawId layer a = traceDraw layer a a

-- | Trace and draw an a on the given layer, while returning a b.
--
-- this will add to the current layer
traceDraw           :: (Show a,Draw a) => LayerName -> a -> b -> b
traceDraw layer a b = unsafePerformIO $ traceDrawIO layer a b
{-# NOINLINE traceDraw #-}

-- | Implementation of traceDraw
traceDrawIO           :: (Show a,Draw a) => LayerName -> a -> b -> IO b
traceDrawIO layer a b = b <$ debugClient (clientDrawLayer (layer,show a, draw' a))

-- | Clears a particular layer
clearLayer      :: LayerName -> IO ()
clearLayer layer = debugClient $ clientClearLayer layer

-- | Clears all layers
clear :: IO ()
clear = debugClient clientClear


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

clientDrawing :<|> clientDrawLayer :<|> clientClearLayer :<|> clientClear = client (Proxy @API)


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
defaultBaseUrl = BaseUrl Http "localhost" defaultPort ""

defaultPort :: Port
defaultPort = 8000



--------------------------------------------------------------------------------

type API =    "drawing"    :> Get '[ PlainText ] String
         :<|> "drawLayer"  :> ReqBody '[ JSON ] (LayerName, String, Drawing) :> Put '[JSON] ()
         :<|> "clearLayer" :> ReqBody '[ PlainText ] LayerName               :> Put '[JSON] ()
         :<|> "clear"      :> Put '[JSON] ()

  -- "traceDraw" :>


--------------------------------------------------------------------------------

data Drawing = Drawing -- dummy
  deriving (Generic,Show)


instance ToJSON Drawing
instance FromJSON Drawing

data Svg = Svg


data MisoHtml

type State = Map LayerName (Seq (String, Drawing))

-- dummy
-- serverState = State $ Map.fromList [("dummy",Drawing)]

type StateRef = IORef State


type Server' api = ServerT api Handler'


newtype Handler' a = Handler' {unHandler' :: ReaderT StateRef IO a}
                   deriving (Functor, Applicative, Monad, MonadReader StateRef, MonadIO)

--------------------------------------------------------------------------------

instance MonadState State Handler' where
  state f = do ref <- ask
               liftIO $ atomicModifyIORef ref (swap . f)


server :: Server' API
server =   handleDrawing
      :<|> handleDrawLayer
      :<|> handleClearLayer
      :<|> handleClear

handleDrawing :: Handler' String
handleDrawing = gets show

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
    api = Proxy @API

main :: IO ()
main = runDebugServer defaultPort $ do
  print "woei"
  debugClient $ clientDrawLayer ("myLayer","my layer content", Drawing)
  x <- read <$> getLine
  print $ fib x

fib :: Int -> Int
fib = \case
  0 -> 0
  1 -> 1
  x -> traceDrawId "fib" $ fib (x-1) + fib (x-2)
