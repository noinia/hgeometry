{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Yaml.Util
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
-- Description :  Helper functions for working with yaml
--
--------------------------------------------------------------------------------
module Data.Yaml.Util( encodeYaml, encodeYamlFile
                     , decodeYaml, decodeYamlFile
                     , printYaml
                     , parseVersioned
                     , Versioned(Versioned), unversioned
                     ) where

import           Control.Applicative
import           Data.Aeson
import           Data.Aeson.Types (typeMismatch)
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
encodeYaml = YamlP.encodePretty encoderConfig

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



--------------------------------------------------------------------------------

-- | Encoder Configuration that we want to use.
encoderConfig :: YamlP.Config
encoderConfig = YamlP.setConfCompare compare YamlP.defConfig
                -- sort fields alphabetically

--------------------------------------------------------------------------------


-- | Data type for things that have a version
data Versioned a = Versioned { version :: Version
                             , content :: a
                             } deriving (Show,Read,Generic,Eq,Functor,Foldable,Traversable)

unversioned :: Versioned a -> a
unversioned = content

instance ToJSON a => ToJSON (Versioned a) where
  toJSON     (Versioned v x) = object [ "version" .= showVersion v, "content" .= x]
  toEncoding (Versioned v x) = pairs ("version" .= showVersion v <> "content" .= x)


-- | Given a list of candidate parsers, select the right one
parseVersioned               :: [(Version -> Bool,Value -> Parser a)]
                             -> Value -> Parser (Versioned a)
parseVersioned ps (Object o) = do V v <- o .: "version"
                                  co  <- o .: "content"
                                  let ps' = map (\(_,p) -> Versioned v <$> p co)
                                          . filter (($ v) . fst) $ ps
                                      err = fail $ "no matching version found for version "
                                                   <> showVersion v
                                  foldr (<|>) err ps'
parseVersioned _ invalid     = typeMismatch "Versioned" invalid

-- instance (FromJSON a) => FromJSON (Versioned a) where
--   parseJSON (Object v) = Versioned <$> (unV <$> v .: "version")
--                                    <*> v .: "content"
--   parseJSON invalid    = typeMismatch "Versioned" invalid

newtype V = V Version

instance FromJSON V where
  parseJSON (String t) = case filter (null . snd) (readP_to_S parseVersion $ T.unpack t) of
     ((v,""):_) -> pure $ V v
     _          -> fail $ "parsing " <> show t <> " into a version failed"
  parseJSON invalid    = typeMismatch "Version" invalid
