module Data.Yaml.Util where

import qualified Data.Yaml.Pretty as YamlP
import           Data.Yaml
import           Data.ByteString (ByteString)
import           Data.ByteString.Char8 as B

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
