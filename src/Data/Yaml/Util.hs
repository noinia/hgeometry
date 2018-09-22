{-# LANGUAGE OverloadedStrings #-}
module Data.Yaml.Util( encodeYaml, encodeYamlFile
                     , decodeYaml, decodeYamlFile
                     , printYaml
                     , Versioned(Versioned)
                     ) where

import           Data.Aeson
import           Data.Aeson.Types(typeMismatch)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import           Data.Version
import           Data.Yaml
import qualified Data.Yaml.Pretty as YamlP
import           GHC.Generics (Generic)
import           Text.ParserCombinators.ReadP (readP_to_S)

--------------------------------------------------------------------------------

-- | Write the output to yaml
encodeYaml :: ToJSON a => a -> ByteString
encodeYaml = YamlP.encodePretty YamlP.defConfig

-- | Prints the yaml
printYaml :: ToJSON a => a -> IO ()
printYaml = B.putStrLn . encodeYaml

-- | alias for decodeEither' from the Yaml Package
decodeYaml :: FromJSON a => ByteString -> Either ParseException a
decodeYaml = decodeEither'

-- | alias for reading a yaml file
decodeYamlFile :: FromJSON a => FilePath -> IO (Either ParseException a)
decodeYamlFile = decodeFileEither

-- | Encode a yaml file
encodeYamlFile    :: ToJSON a => FilePath -> a -> IO ()
encodeYamlFile fp = B.writeFile fp . encodeYaml


-- | Data type for things that have a version
data Versioned a = Versioned { version :: Version
                             , content :: a
                             } deriving (Show,Read,Generic,Eq,Functor,Foldable,Traversable)


instance ToJSON a => ToJSON (Versioned a) where
  toJSON     (Versioned v x) = object [ "version" .= showVersion v, "content" .= x]
  toEncoding (Versioned v x) = pairs ("version" .= showVersion v <> "content" .= x)

instance (FromJSON a) => FromJSON (Versioned a) where
  parseJSON (Object v) = Versioned <$> (unV <$> v .: "version")
                                   <*> v .: "content"
  parseJSON invalid    = typeMismatch "Versioned" invalid

newtype V = V { unV :: Version }

instance FromJSON V where
  parseJSON (String t) = case filter (null . snd) (readP_to_S parseVersion $ T.unpack t) of
     ((v,""):_) -> pure $ V v
     _          -> fail $ "parsing " <> show t <> " into a version failed"
  parseJSON invalid    = typeMismatch "Version" invalid
