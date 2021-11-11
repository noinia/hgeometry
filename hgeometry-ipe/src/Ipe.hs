--------------------------------------------------------------------------------
-- |
-- Module      :  Ipe
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Reexports the functionality for reading and writing Ipe files.
--
--------------------------------------------------------------------------------
module Ipe(
  -- * Ipe Files
    IpeFile(IpeFile), preamble, styles, pages
  , ipeFile, singlePageFile, singlePageFromContent
  -- ** Reading Ipe files
  , readIpeFile
  , readSinglePageFile
  , readSinglePageFileThrow
  , readRawIpeFile
  , ConversionError
  -- *** Reading all Geometries from a single page ipe file
  , readAll, readAllFrom
  -- ** Writing ipe files
  , writeIpeFile, writeIpeFile', writeIpePage
  , toIpeXML
  , printAsIpeSelection, toIpeSelectionXML


  -- * Ipe Pages
  , IpePage(IpePage), layers, views, content
  , emptyPage, fromContent
  , onLayer, contentInView
  , withDefaults

  -- * Content: Ipe Objects
  , IpeObject(..), _IpePath, _IpeUse, _IpeGroup, _IpeTextLabel, _IpeMiniPage, _IpeImage
  , IpeObject'
  , ipeObject'
  , ToObject(..)
  -- ** Specific Ipe-Objects
  , Path(Path), pathSegments
  , PathSegment(..)
  , IpeSymbol(Symbol), symbolPoint, symbolName
  , Group(Group), groupItems
  , TextLabel(..)
  , MiniPage(..), width
  , Image(Image), imageData, rect
  , IpeBitmap
  -- ** Attributes
  , IpeAttributes
  , Attributes', AttributesOf, AttrMap, AttrMapSym1
  , attributes, traverseIpeAttrs
  , commonAttributes
  -- * Layers and Views
  , LayerName(LayerName), layerName
  , View(View), layerNames, activeLayer
  -- * Ipe Syles and Preamble
  , IpeStyle(IpeStyle), styleName, styleData
  , basicIpeStyle
  , readIpeStylesheet
  , addStyleSheetFrom
  , IpePreamble(IpePreamble), encoding, preambleData
  -- * Reading Geometries *From* Ipe
  , IpeRead(..)
  -- ** Converting *from* IpeObjects
  , _asPoint
  , _asLineSegment
  , _asRectangle
  , _asTriangle
  , _asPolyLine
  , _asSomePolygon, _asSimplePolygon, _asMultiPolygon
  -- *** Dealing with Attributes
  , _withAttrs
  -- ** Default readers
  , HasDefaultFromIpe(..)

  -- * Converting *to* IpeObjects
  -- ** IpeWrite
  , IpeWrite(..)
  , IpeWriteText(..)
  -- ** IpeOut
  , module Ipe.IpeOut




  , module Ipe.Types
  -- , module Ipe.Writer
  -- , module Ipe.Reader
  , module Ipe.FromIpe
  , module Ipe.Attributes
  , module Ipe.Value
  , module Ipe.Color
  ) where

import Ipe.Types
import Ipe.Writer
import Ipe.Reader
import Ipe.IpeOut
import Ipe.FromIpe
import Ipe.Attributes
import Ipe.Value
import Ipe.Color(IpeColor(..))
