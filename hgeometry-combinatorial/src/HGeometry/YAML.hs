{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.YAML
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
-- Description :  Helper functions for working with yaml
--
--------------------------------------------------------------------------------
module HGeometry.YAML
  ( encodeYAML, encodeYAMLFile
  , decodeYAML, decodeYAMLFile
  , printYAML
  , Versioned(Versioned), unversioned
  ) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as Char8
import qualified Data.Text as Text
import           Data.Version
import           Data.YAML
import           GHC.Generics (Generic)
import qualified System.File.OsPath as File
import           System.OsPath
import           Text.ParserCombinators.ReadP (readP_to_S)

--------------------------------------------------------------------------------

-- | Write the output to yaml
encodeYAML :: ToYAML a => a -> ByteString
encodeYAML = encode1Strict

-- | Prints the yaml
printYAML :: ToYAML a => a -> IO ()
printYAML = Char8.putStrLn . encodeYAML

-- | alias for decodeEither' from the YAML Package
decodeYAML :: FromYAML a => ByteString -> Either (Pos, String) a
decodeYAML = decode1Strict

-- | alias for reading a yaml file
decodeYAMLFile :: FromYAML a => OsPath -> IO (Either (Pos, String) a)
decodeYAMLFile = fmap decodeYAML . File.readFile'

-- | Encode a yaml file
encodeYAMLFile    :: ToYAML a => OsPath -> a -> IO ()
encodeYAMLFile fp = File.writeFile' fp . encodeYAML

--------------------------------------------------------------------------------

-- | Data type for things that have a version
data Versioned a = Versioned { version :: Version
                             , content :: a
                             } deriving (Show,Read,Generic,Eq,Functor,Foldable,Traversable)

-- | Unpack versioned data type.
unversioned :: Versioned a -> a
unversioned = content

instance ToYAML a => ToYAML (Versioned a) where
  toYAML (Versioned v x) = mapping [ "version" .= Text.pack (showVersion v)
                                   , "content" .= x
                                   ]

instance FromYAML a => FromYAML (Versioned a) where
  parseYAML = withMap "Versioned" $ \m -> Versioned <$> (unV <$> m .: "version")
                                                    <*> m .: "content"


-- -- | Given a list of candidate parsers, select the right one
-- parseVersioned               :: [(Version -> Bool,Value -> Parser a)]
--                              -> Value -> Parser (Versioned a)
-- parseVersioned ps (Object o) = do V v <- o .: "version"
--                                   co  <- o .: "content"
--                                   let ps' = map (\(_,p) -> Versioned v <$> p co)
--                                           . filter (($ v) . fst) $ ps
--                                       err = fail $ "no matching version found for version "
--                                                    <> showVersion v
--                                   foldr (<|>) err ps'
-- parseVersioned _ invalid     = typeMismatch "Versioned" invalid

newtype V = V { unV :: Version }

parseVersion'   :: Text.Text -> Parser V
parseVersion' t = case filter (null . snd) (readP_to_S parseVersion $ Text.unpack t) of
                    ((v,""):_) -> pure $ V v
                    _          -> fail $ "parsing " <> show t <> " into a version failed"

instance FromYAML V where
  parseYAML = withStr "version" parseVersion'


-- instance FromYAML V where
--   parseYAML (String t) = case filter (null . snd) (readP_to_S parseVersion $ T.unpack t) of
--      ((v,""):_) -> pure $ V v
--      _          -> fail $ "parsing " <> show t <> " into a version failed"
--   parseYAML invalid    = typeMismatch "Version" invalid
