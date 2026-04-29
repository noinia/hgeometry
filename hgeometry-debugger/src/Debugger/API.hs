module Debugger.API
  ( API
  , ServerAPI

  , LayerName
  , Drawing(Drawing)

  , defaultHost
  , defaultPort
  ) where

import Servant.API
import GHC.Generics
import Data.Aeson
import Data.Text (Text)

--------------------------------------------------------------------------------


type LayerName = String

newtype Drawing = Drawing Text
  deriving (Generic, Eq, Show)

instance ToJSON Drawing where
instance FromJSON Drawing where


type API =    "drawing"    :> Get '[ PlainText ] String
         :<|> "drawLayer"  :> ReqBody '[ JSON ] (LayerName, String, Drawing) :> Put '[JSON] ()
         :<|> "clearLayer" :> ReqBody '[ PlainText ] LayerName               :> Put '[JSON] ()
         :<|> "clear"      :> Put '[JSON] ()

type ServerAPI = "pub"        :> Raw
         :<|> API

--------------------------------------------------------------------------------

defaultPort :: Int -- Port
defaultPort = 8000

defaultHost :: String
defaultHost = "localhost"
