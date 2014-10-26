{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.Geometry.Ipe.Reader where

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


fromIpeXML = undefined

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

validateAll :: err -> Prism' (Operation r) (Point 2 r) -> [Operation r]
               -> Either err [Point 2 r]
validateAll = undefined -- TODO: Implement this, using Validation

toValidator     :: err -> Maybe a -> AccValidation [err] a
toValidator err = maybe (_Failure # [err]) (_Success #)
