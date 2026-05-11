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
import           Miso.Aeson
import qualified Data.Aeson as Aeson
import qualified Miso.JSON as Miso
import           Data.Coerce
import           Data.Map (Map)
import           Data.Sequence (Seq)

--------------------------------------------------------------------------------


type LayerName = String

newtype Drawing = Drawing MisoString
  deriving (Generic, Eq, Show)
  deriving newtype (Miso.FromJSON, Miso.ToJSON)
  deriving (Aeson.ToJSON, Aeson.FromJSON) via MisoAeson Drawing

instance (Aeson.ToJSONKey k, Aeson.ToJSON k, Aeson.ToJSON v) => Miso.ToJSON (Map k v) where
  toJSON = Miso.toJSON . MisoAeson

instance (Aeson.FromJSONKey k, Ord k, Aeson.FromJSON v
         ) => Miso.FromJSON (Map k v) where
  parseJSON = fmap (\(MisoAeson x) -> x) . Miso.parseJSON

instance Miso.ToJSON a => Aeson.ToJSON (MisoAeson a) where
  toJSON (MisoAeson x) = jsonToAeson . Miso.toJSON $ x

instance Miso.FromJSON a => Aeson.FromJSON (MisoAeson a) where
  parseJSON s = case Miso.unParser . Miso.parseJSON . aesonToJSON $ s of
                  Left err -> fail (show err)
                  Right x  -> pure (MisoAeson x)


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

-- deriving instance (Generic action, Generic model) => Generic (View model action)
-- deriving instance (Generic parent, Generic model, Generic action
--                   ) => Generic (Component parent model action)
-- instance (Generic parent) => Generic (SomeComponent parent) where



-- data ViewJSON =

-- instance (ToJSON model, ToJSON action) => ToJSON (View model action) where
--    toEncoding =


--      genericToEncoding defaultOptions
