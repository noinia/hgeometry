module Debugger.API
  ( API

  , LayerName
  , Drawing(Drawing)
  ) where

import Servant.API
import GHC.Generics
import Data.Aeson
import Data.Text (Text)
import Data.Proxy

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
