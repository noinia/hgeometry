module Debugger.API
  ( API

  , LayerName
  , Drawing


  , clientDrawing
  , clientDrawLayer
  , clientClearLayer
  , clientClear
  ) where

import Servant
import GHC.Generics
import Data.Aeson
import           Servant.Client ( ClientM, runClientM, ClientEnv, mkClientEnv
                                , BaseUrl(..), client, Scheme(..)
                                )

--------------------------------------------------------------------------------


type LayerName = String

newtype Drawing = Drawing Text
  deriving (Generic, Show)

instance ToJSON Drawing where
instance FromJSON Drawing where


type API =    "pub"        :> Raw
         :<|> "drawing"    :> Get '[ PlainText ] String
         :<|> "drawLayer"  :> ReqBody '[ JSON ] (LayerName, String, Drawing) :> Put '[JSON] ()
         :<|> "clearLayer" :> ReqBody '[ PlainText ] LayerName               :> Put '[JSON] ()
         :<|> "clear"      :> Put '[JSON] ()



--------------------------------------------------------------------------------

clientStatic
  :<|> clientDrawing
  :<|> clientDrawLayer
  :<|> clientClearLayer
  :<|> clientClear
  = client (Proxy @API)
