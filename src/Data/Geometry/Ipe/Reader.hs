{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.Geometry.Ipe.Reader where

import           Data.Either(rights)
import           Control.Applicative
import           Control.Lens

import           Data.Ext
import qualified Data.Foldable as F
import           Data.Vinyl

import           Data.Validation

import           Data.Geometry.Point
import           Data.Geometry.PolyLine

import           Data.Geometry.Ipe.Types

import qualified Data.ByteString as B
import           Data.Monoid
import           Data.Text(Text)

import           Data.Geometry.Ipe.PathParser

import           Text.XML.Expat.Tree

import qualified Data.Text as T

-- fromIpeFile :: (Coordinate r, IpeRead t) => FilePath -> IO [PolyLine 2 () r]
-- fromIpeFile


fromIpeXML   :: (Coordinate r, IpeRead t) => B.ByteString -> Either ConversionError (t r)
fromIpeXML b = (bimap (T.pack . show) id $ parse' defaultParseOptions b) >>= ipeRead

class IpeReadText t where
  ipeReadText :: Coordinate r => Text -> Either ConversionError (t r)

type ConversionError = Text

-- TODO: We also want to do something with the attributes

class IpeRead t where
  ipeRead :: Coordinate r => Node Text Text -> Either ConversionError (t r)

-- instance IpeRead IpeSymbol where
--   ipeRead (Element "use" ats _) = case extract ["pos","name"] ats of

-- given a list of keys, and a list of attributes. Extracts the values matching
-- the keys. The result is a pair (vs,others), where others are the remaining
-- attributes, and vs are the values corresponding to the keys. Sorted on
-- increasing order of their keys.
extract :: [Text] -> [(Text,Text)] -> ([Text],[(Text,Text)])
extract = undefined

instance IpeReadText (PolyLine 2 ()) where
  ipeReadText t = readPathOperations t >>= fromOps
    where
      fromOps (MoveTo p:LineTo q:ops) = (\ps -> fromPoints $ [p,q] ++ ps)
                                     <$> validateAll "Expected LineTo p" _LineTo ops
      fromOps _                       = Left "Expected MoveTo p:LineTo q:... "

validateAll         :: ConversionError -> Prism' (Operation r) (Point 2 r) -> [Operation r]
                    -> Either ConversionError [Point 2 r]
validateAll err fld = bimap T.unlines id . validateAll' err fld


validateAll' :: err -> Prism' (Operation r) (Point 2 r) -> [Operation r]
               -> Either [err] [Point 2 r]
validateAll' err field = toEither . foldr (\op res -> f op <> res) (Right' [])
  where
    f op = maybe (Left' [err]) (\p -> Right' [p]) $ op ^? field
    toEither = either' Left Right

-- This is a bit of a hack
instance IpeRead (PolyLine 2 ()) where
  ipeRead (Element "path" ats ts) = ipeReadText . T.unlines . map unText $ ts
                                    -- apparently hexpat already splits the text into lines
  ipeRead _                       = Left "iperead: no polyline."

unText (Text t) = t


instance IpeRead PathSegment where
  ipeRead = fmap PolyLineSegment . ipeRead

testP :: B.ByteString
testP = "<path stroke=\"black\">\n128 656 m\n224 768 l\n304 624 l\n432 752 l\n</path>"

testO :: Text
testO = "\n128 656 m\n224 768 l\n304 624 l\n432 752 l\n"

testPoly :: Either Text (PolyLine 2 () Double)
testPoly = fromIpeXML testP

-- ipeRead' :: [Element Text Text]
-- ipeRead' = map ipeRead

-- instance IpeRead (IpePage gs) where
--   ipeRead (Element "page" ats chs) = Right . IpePage [] [] . fromList' . rights $ map ipeRead chs
--     where
--       fromList' = Group' . foldr (\x r -> (IpeObject x :& RNil) :& r) RNil
--   ipeRead _                        = Left "ipeRead: Not a page"

readPolyLines :: Coordinate r => Node Text Text -> [PolyLine 2 () r]
readPolyLines (Element "ipe" _ chs) = concatMap readPolyLines' chs


readPolyLines' :: Coordinate r => Node Text Text -> [PolyLine 2 () r]
readPolyLines' (Element "page" _ chs) = rights $ map ipeRead chs
readPolyLines' _                      = []

polylinesFromIpeFile :: (Coordinate r) => FilePath -> IO [PolyLine 2 () r]
polylinesFromIpeFile = fmap readPolies . B.readFile
  where
    readPolies = either (const []) readPolyLines . parse' defaultParseOptions
