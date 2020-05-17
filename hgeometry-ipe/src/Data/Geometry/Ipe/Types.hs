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
  , emptyPage, fromContent
  , onLayer, contentInView
  , withDefaults

  , IpeFile(IpeFile), preamble, styles, pages
  , ipeFile, singlePageFile, singlePageFromContent
  ) where


import           Control.Lens hiding (views)
import           Data.Geometry.Ipe.Attributes hiding (Matrix)
import           Data.Geometry.Ipe.Content
import           Data.Geometry.Ipe.Layer
import           Data.Geometry.Ipe.Literal
import qualified Data.List.NonEmpty as NE
import           Data.Maybe (mapMaybe)
import           Data.Semigroup (Endo)
import qualified Data.Set as Set
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

-- | Creates an empty page with one layer and view.
emptyPage :: IpePage r
emptyPage = fromContent []

-- | Creates a simple page with a single view.
fromContent     :: [IpeObject r] -> IpePage r
fromContent obs = IpePage layers' [View layers' a] obs
  where
    layers' = Set.toList . Set.fromList $ a : mapMaybe (^.commonAttributes.ixAttr SLayer) obs
    a       = "alpha"

-- | Makes sure that the page has at least one layer and at least one
-- view, essentially matching the behaviour of ipe. In particular,
--
-- - if the page does not have any layers, it creates a layer named "alpha", and
-- - if the page does not have any views, it creates a view in which all layers are visible.
--
withDefaults :: IpePage r -> IpePage r
withDefaults = addView . addLayer
  where
    whenNull ys = \case
                    [] -> ys
                    xs -> xs
    addLayer p = p&layers %~ whenNull ["alpha"]
    addView  p = p&views  %~ whenNull [View (p^.layers) (head $ p^.layers)]
                 -- note that the head is save, since we just made sure
                 -- with 'addLayer' that there is at least one layer

-- | This allows you to filter the objects on some layer.
--
-- >>> let page = IpePage [] [] []
-- >>> page^..content.onLayer "myLayer"
-- []
onLayer   :: LayerName -> Getting (Endo [IpeObject r]) [IpeObject r] (IpeObject r)
onLayer n = folded.filtered (\o -> o^?commonAttributes._Attr SLayer == Just n)

-- | Gets all objects that are visible in the given view.
--
-- Note that views are indexed starting from 0. If the page does not
-- have any explicit view definitions, this function returns an empty
-- list.
--
-- >>> let page = IpePage [] [] []
-- >>> page^.contentInView 0
-- []
contentInView                     :: Word -> Getter (IpePage r) [IpeObject r]
contentInView (fromIntegral -> i) = to inView'
  where
    inView' p = let lrs = Set.fromList . concatMap (^.layerNames) $ p^..views.ix i
                in p^..content.folded.filtered (inVisibleLayer lrs)

    inVisibleLayer lrs o = maybe False (`Set.member` lrs) $ o^?commonAttributes._Attr SLayer

--------------------------------------------------------------------------------

-- | A complete ipe file
data IpeFile r = IpeFile { _preamble :: Maybe IpePreamble
                         , _styles   :: [IpeStyle]
                         , _pages    :: NE.NonEmpty (IpePage r)
                         }
               deriving (Eq,Show)
makeLenses ''IpeFile


-- | Convenience constructor for creating an ipe file without preamble
-- and with the default stylesheet.
ipeFile :: NE.NonEmpty (IpePage r) -> IpeFile r
ipeFile = IpeFile Nothing [basicIpeStyle]

-- | Convenience function to construct an ipe file consisting of a single page.
singlePageFile :: IpePage r -> IpeFile r
singlePageFile = ipeFile . (NE.:| [])

-- | Create a single page ipe file from a list of IpeObjects
singlePageFromContent :: [IpeObject r] -> IpeFile r
singlePageFromContent = singlePageFile . fromContent
