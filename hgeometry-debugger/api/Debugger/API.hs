module Debugger.API
  ( API
  , ServerAPI

  , LayerName
  , Drawing(Drawing)
  , Drawings

  , defaultHost
  , defaultPort
  ) where

import           Servant.API
import           GHC.Generics
import           Data.Aeson
import           Data.Text (Text)
import           Miso.Types
import qualified Data.Aeson as Aeson
import           Data.Coerce
import           Data.Map (Map)
import           Data.Sequence (Seq)

--------------------------------------------------------------------------------


type LayerName = String

newtype Drawing = Drawing MisoString
  deriving (Generic, Eq, Show)

instance Aeson.ToJSON Drawing where
instance Aeson.FromJSON Drawing where


type Drawings = Map LayerName (Seq (String, Drawing))

type API =    "drawing"    :> Get '[ JSON ] Drawings
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




--------------------------------------------------------------------------------

-- deriving instance (Generic action, Generic model) => Generic (View context props model action)
-- deriving instance (Generic parent, Generic model, Generic action
--                   ) => Generic (Component parent model action)
-- instance (Generic parent) => Generic (SomeComponent parent) where



-- data ViewJSON =

-- instance (ToJSON model, ToJSON action) => ToJSON (View context props model action) where
--    toEncoding =


--      genericToEncoding defaultOptions
