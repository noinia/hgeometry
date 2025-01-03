{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Ipe.Types
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Data type modeling the various elements in Ipe files.
--
--------------------------------------------------------------------------------
module Ipe.Types(
  -- * Ipe Files
    IpeFile(IpeFile), preamble, styles, pages
  , ipeFile, singlePageFile, singlePageFromContent
  -- * Ipe Pages
  , IpePage(IpePage), layers, views, content
  , emptyPage, fromContent
  , onLayer, contentInView
  , withDefaults
  -- * Content: Ipe Objects
  , IpeObject(..), _IpeGroup, _IpeImage, _IpeTextLabel, _IpeMiniPage, _IpeUse, _IpePath
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
  -- * Attributes
  , IpeAttributes
  , Attributes', AttributesOf, AttrMap, AttrMapSym1
  , attributes, traverseIpeAttrs
  , commonAttributes
  -- * Layers and Views
  , LayerName(LayerName), layerName
  , View(View), layerNames, activeLayer
  -- * Styles and Preamble
  , addStyleSheet
  , IpeStyle(IpeStyle), styleName, styleData
  , basicIpeStyle, opacitiesStyle
  , IpePreamble(IpePreamble), encoding, preambleData
  --
  -- , flattenGroups
  ) where


import           Control.Lens hiding (views)
import qualified Data.List.NonEmpty as NE
import           Data.Maybe (mapMaybe)
import           Data.Semigroup (Endo)
import qualified Data.Set as Set
import           Data.Text (Text)
import           GHC.Generics (Generic)
import           Ipe.Attributes hiding (Matrix)
import           Ipe.Content
import           Ipe.Layer
import           Ipe.Literal
import           Text.XML.Expat.Tree (Node)


--------------------------------------------------------------------------------


-- | The definition of a view
-- make active layer into an index ?
data View = View { _layerNames      :: [LayerName]
                 , _activeLayer     :: LayerName
                 }
          deriving (Eq, Ord, Show, Generic)

-- | Lens to access the layers in this view
layerNames :: Lens' View [LayerName]
layerNames f (View ns a) = fmap (\ns' -> View ns' a) (f ns)
{-# INLINE layerNames #-}

-- | Lens to access the active layer
activeLayer               :: Lens' View LayerName
activeLayer f (View ns a) = fmap (\a' -> View ns a') (f a)
{-# INLINE activeLayer #-}

-- instance Default


-- | for now we pretty much ignore these
data IpeStyle = IpeStyle { _styleName :: Maybe Text
                         , _styleData :: Node Text Text
                         }
              deriving (Eq,Show,Generic)

-- | Lens to access the style name
styleName :: Lens' IpeStyle (Maybe Text)
styleName f (IpeStyle n sd) = fmap (\n' -> IpeStyle n' sd) (f n)
{-# INLINE styleName #-}

-- | Lens to access the style data
styleData :: Lens' IpeStyle (Node Text Text)
styleData f (IpeStyle n sd) = fmap (\sd' -> IpeStyle n sd') (f sd)
{-# INLINE styleData #-}

-- | The "basic" ipe stylesheet
basicIpeStyle :: IpeStyle
basicIpeStyle = IpeStyle (Just "basic") (xmlLiteral [litFile|data/ipe/basic.isy|])

-- | A stylesheet with some convenient predefined opacities. In particular
-- the opacities "10%","20%",..,"90%".
opacitiesStyle :: IpeStyle
opacitiesStyle = IpeStyle (Just "opacities") (xmlLiteral [litFile|data/ipe/opacities.isy|])

-- | The maybe string is the encoding
data IpePreamble  = IpePreamble { _encoding     :: Maybe Text
                                , _preambleData :: Text
                                }
                  deriving (Eq,Read,Show,Ord,Generic)

-- | Lens to access the encoding
encoding :: Lens' IpePreamble (Maybe Text)
encoding f (IpePreamble e pd) = fmap (\e' -> IpePreamble e' pd) (f e)
{-# INLINE encoding #-}

-- | Lens to access the preambleData
preambleData :: Lens' IpePreamble Text
preambleData f (IpePreamble e pd) = fmap (\pd' -> IpePreamble e pd') (f pd)
{-# INLINE preambleData #-}

-- | Ipe Bitmap data
type IpeBitmap = Text

--------------------------------------------------------------------------------
-- Ipe Pages

-- | An IpePage is essentially a Group, together with a list of layers and a
-- list of views.
data IpePage r = IpePage { _layers  :: [LayerName]
                         , _views   :: [View]
                         , _content :: [IpeObject r]
                         }
              deriving (Eq,Show,Generic)

-- | Lens to access the layers of an ipe page
layers :: Lens' (IpePage r) [LayerName]
layers f (IpePage lrs vs cnts) = fmap (\lrs' -> IpePage lrs' vs cnts) (f lrs)
{-# INLINE layers #-}

-- | Lens to access the views of an ipe page
views :: Lens' (IpePage r) [View]
views f (IpePage lrs vs cnts) = fmap (\vs' -> IpePage lrs vs' cnts) (f vs)
{-# INLINE views #-}

-- | Lens to access the content of an ipe page
content :: Lens (IpePage r) (IpePage r') [IpeObject r] [IpeObject r']
content f (IpePage lrs vs cnts) = fmap (\cnts' -> IpePage lrs vs cnts') (f cnts)
{-# INLINE content #-}

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
withDefaults   :: IpePage r -> IpePage r
withDefaults p = case p^.layers of
                   []      -> makeNonEmpty "alpha" []
                   (l:lrs) -> makeNonEmpty l       lrs
  where
    makeNonEmpty l lrs = p&layers .~ (l:lrs)
                          &views %~ \case
                                      [] -> [View (l:lrs) l]
                                      vs -> vs

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
               deriving (Eq,Show,Generic)

-- | Lens to access the preamble of an ipe file
preamble :: Lens' (IpeFile r) (Maybe IpePreamble)
preamble f (IpeFile p ss pgs) = fmap (\p' -> IpeFile p' ss pgs) (f p)
{-# INLINE preamble #-}

-- | Lens to access the styles of an ipe file
styles :: Lens' (IpeFile r) [IpeStyle]
styles f (IpeFile p ss pgs) = fmap (\ss' -> IpeFile p ss' pgs) (f ss)
{-# INLINE styles #-}

-- | Lens to access the pages of an ipe file
pages :: Lens (IpeFile r) (IpeFile r') (NE.NonEmpty (IpePage r)) (NE.NonEmpty (IpePage r'))
pages f (IpeFile p ss pgs) = fmap (\pgs' -> IpeFile p ss pgs') (f pgs)
{-# INLINE pages #-}

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


-- | Adds a stylesheet to the ipe file. This will be the first
-- stylesheet, i.e. it has priority over all previously imported stylesheets.
addStyleSheet     :: IpeStyle -> IpeFile r -> IpeFile r
addStyleSheet s f = f&styles %~ (s:)
