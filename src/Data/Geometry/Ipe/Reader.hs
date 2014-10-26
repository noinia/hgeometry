{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.Geometry.Ipe.Reader where

import           Data.Ext
import qualified Data.Foldable as F
import           Data.Vinyl

import           Data.Geometry.Point
import           Data.Geometry.PolyLine

import           Data.Geometry.Ipe.Types

import qualified Data.ByteString as B
import           Data.Monoid
import           Data.Text(Text)

import Data.Geometry.Ipe.PathParser

import           Text.XML.Expat.Tree

import qualified Data.Text as T


fromIpeXML = undefined

class IpeReadText t where
  ipeReadText :: Text -> t






class IpeRead t where
  ipeReadText :: Node Text Text -> t
