{-# OPTIONS_GHC -Wno-orphans #-}
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
  , readAll, readAllDeep, readAllFrom
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
  , basicIpeStyle, opacitiesStyle
  , readIpeStylesheet
  , addStyleSheet, addStyleSheetFrom
  , IpePreamble(IpePreamble), encoding, preambleData
  -- * Reading Geometries *From* Ipe
  , IpeRead(..)
  -- ** Converting *from* IpeObjects
  , _asPoint
  , _asLineSegment
  , _asClosedLineSegment
  , _asHalfLine
  , _asRectangle
  , _asTriangle
  , _asPolyLine
  , _asSimplePolygon
  , _asConvexPolygon
  , _asPolygonalDomain

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
  -- ** Batch reexports

  -- , module Ipe.Types
  -- , module Ipe.FromIpe
  , module Ipe.Attributes
  , module Ipe.Value
  , IpeColor(..), named
  ) where

import Control.Lens hiding (views)
import Data.Maybe (isJust)
import HGeometry.Ext
import HGeometry.HalfLine
import HGeometry.LineSegment
import HGeometry.Point
import Ipe.Attributes
import Ipe.Color
import Ipe.FromIpe
import Ipe.IpeOut
import Ipe.Reader
import Ipe.Types
import Ipe.Value
import Ipe.Writer

--------------------------------------------------------------------------------

-- | Try to parse an Line segment with an arrow head as a HalfLine
_asHalfLine :: (Fractional r, Ord r, Show r)
            => Prism' (IpeObject r) (HalfLine (Point 2 r) :+ IpeAttributes Path r)
_asHalfLine = prism' (\(hl :+ ats) -> IpePath (ipeHalfLine hl ! ats)) objToHalfLine
  where
    objToHalfLine = \case
      IpePath (path' :+ ats) -> case path'^?_asClosedLineSegment  of
        Just (ClosedLineSegment s t) -> case (hasAttr SArrow ats, hasAttr SRArrow ats) of
                                          (True,False) -> Just $ HalfLine s (t .-. s) :+ ats
                                          (False,True) -> Just $ HalfLine s (s .-. t) :+ ats
                                          _            -> Nothing
        Nothing                      -> Nothing
      _                    -> Nothing

    hasAttr a = isJust . lookupAttr a


instance (Fractional r, Ord r, Show r) => HasDefaultFromIpe (HalfLine (Point 2 r)) where
  type DefaultFromIpe (HalfLine (Point 2 r)) = Path
  defaultFromIpe = _asHalfLine
