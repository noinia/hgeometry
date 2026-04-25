module Main(main) where

import           Data.Tuple (swap)
import           Control.Monad.Reader
import           Control.Monad.State.Class
import           Data.IORef
import           Miso
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
--------------------------------------------------------------------------------

type LayerName = String

-- type Svg =

class Draw t where
  -- | Draw something as svg
  draw :: t -> View model action

--------------------------------------------------------------------------------
-- * Our API


-- | Trace and draw an a on the given layer, while returning a b.
--
-- this will add to the current layer
traceLayer :: (Show a,Draw a) => LayerName -> a -> b
traceLayer = undefined

-- | Trace and draw an a, attaching the given key
traceDraw :: (Show a,Draw a) => String -> a -> b
traceDraw = undefined

-- | Clears a particular layer
clearLayer :: LayerName -> IO ()
clearLayer = undefined

-- | Clears all layers
clear :: IO ()
clear = undefined


-- | Start the debug server, then do stuff
runDebugServer          :: Int -> IO () -> IO ()
runDebugServer port act = do putStrLn $ "Starting the server on port " <> show port
                             stateRef <- newIORef mempty
                             run port $ app stateRef
                             act

--------------------------------------------------------------------------------

debugStateRef :: StateRef
debugStateRef = unsafePerformIO $ newIORef mempty
{-# NOINLINE debugStateRef #-}

-- TODO: this should get some client I guess

-- debugClient
-- debugClient = client (Proxy @Api)


--------------------------------------------------------------------------------

type API =    "drawing"    :> Get '[ PlainText ] String
         :<|> "drawLayer"  :> ReqBody '[ JSON ] (LayerName, String, Drawing) :> Put '[JSON] ()
         :<|> "draw"       :> ReqBody '[ JSON ] (String,    String, Drawing) :> Put '[JSON] ()
         :<|> "clearLayer" :> ReqBody '[ PlainText ] LayerName               :> Put '[JSON] ()
         :<|> "clear"      :> Put '[JSON] ()

  -- "traceLayer" :>


--------------------------------------------------------------------------------

data Drawing = Drawing -- dummy
  deriving (Generic,Show)


instance ToJSON Drawing
instance FromJSON Drawing

data Svg = Svg


data MisoHtml

type State = Map LayerName [(String, Drawing)]

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
      :<|> handleDraw
      :<|> handleClearLayer
      :<|> handleClear

handleDrawing :: Handler' String
handleDrawing = gets show

handleDraw                         :: (String, String, Drawing) -> Handler' ()
handleDraw (key, content, drawing) =  handleDrawLayer (key, content, drawing)
  -- TODO: make sure the key becomes unique somehow

handleDrawLayer                             :: (LayerName, String, Drawing) -> Handler' ()
handleDrawLayer (layerName,content,drawing) =
  modify $ Map.insertWith (<>) layerName (singleton (content,drawing))

singleton = (:[])


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
main = runDebugServer 8000 $ do print "woei"
