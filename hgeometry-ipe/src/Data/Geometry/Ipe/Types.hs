{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Geometry.Ipe.Types
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Data type modeling the various elements in Ipe files.
--
--------------------------------------------------------------------------------
module Data.Geometry.Ipe.Types(
    LayerName(LayerName), layerName
  , Image(Image), imageData, rect
  , TextLabel(..)
  , MiniPage(..), width

  , IpeSymbol(Symbol), symbolPoint, symbolName

  , Path(Path), pathSegments
  , PathSegment(..)

  , Group(Group), groupItems


  , IpeObject(..), _IpeGroup, _IpeImage, _IpeTextLabel, _IpeMiniPage, _IpeUse, _IpePath
  , IpeObject'
  , ipeObject'
  , ToObject(..)

  , IpeAttributes
  , Attributes', AttributesOf, AttrMap, AttrMapSym1
  , attributes, traverseIpeAttrs
  , commonAttributes

  , flattenGroups


  , View(View), layerNames, activeLayer

  , IpeStyle(IpeStyle), styleName, styleData
  , basicIpeStyle


  , IpePreamble(IpePreamble), encoding, preambleData

  , IpeBitmap


  , IpePage(IpePage), layers, views, content
  , fromContent

  , IpeFile(IpeFile), preamble, styles, pages
  , singlePageFile, singlePageFromContent

  ) where


import           Control.Lens hiding (views)
import           Data.Geometry.Ipe.Attributes hiding (Matrix)
import           Data.Geometry.Ipe.Content
import           Data.Geometry.Ipe.Layer
import           Data.Geometry.Ipe.Literal
import qualified Data.List.NonEmpty as NE
import           Data.Maybe (mapMaybe)
import           Data.Text (Text)
import           Text.XML.Expat.Tree (Node)

--------------------------------------------------------------------------------


-- | The definition of a view
-- make active layer into an index ?
data View = View { _layerNames      :: [LayerName]
                 , _activeLayer     :: LayerName
                 }
          deriving (Eq, Ord, Show)
makeLenses ''View

-- instance Default


-- | for now we pretty much ignore these
data IpeStyle = IpeStyle { _styleName :: Maybe Text
                         , _styleData :: Node Text Text
                         }
              deriving (Eq,Show)
makeLenses ''IpeStyle


basicIpeStyle :: IpeStyle
basicIpeStyle = IpeStyle (Just "basic") (xmlLiteral [litFile|resources/basic.isy|])


-- | The maybe string is the encoding
data IpePreamble  = IpePreamble { _encoding     :: Maybe Text
                                , _preambleData :: Text
                                }
                  deriving (Eq,Read,Show,Ord)
makeLenses ''IpePreamble

type IpeBitmap = Text



--------------------------------------------------------------------------------
-- Ipe Pages

-- | An IpePage is essentially a Group, together with a list of layers and a
-- list of views.
data IpePage r = IpePage { _layers  :: [LayerName]
                         , _views   :: [View]
                         , _content :: [IpeObject r]
                         }
              deriving (Eq,Show)
makeLenses ''IpePage

-- | Creates a simple page with no views.
fromContent     :: [IpeObject r] -> IpePage r
fromContent obs = IpePage layers' [] obs
  where
    layers' = mapMaybe (^.commonAttributes.attrLens SLayer) obs

-- | A complete ipe file
data IpeFile r = IpeFile { _preamble :: Maybe IpePreamble
                         , _styles   :: [IpeStyle]
                         , _pages    :: NE.NonEmpty (IpePage r)
                         }
               deriving (Eq,Show)
makeLenses ''IpeFile

-- | Convenience function to construct an ipe file consisting of a single page.
singlePageFile   :: IpePage r -> IpeFile r
singlePageFile p = IpeFile Nothing [basicIpeStyle] (p NE.:| [])

-- | Create a single page ipe file from a list of IpeObjects
singlePageFromContent :: [IpeObject r] -> IpeFile r
singlePageFromContent = singlePageFile . fromContent
