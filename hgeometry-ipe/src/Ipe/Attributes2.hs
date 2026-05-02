{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuantifiedConstraints #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Ipe.Attributes
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Possible Attributes we can assign to items in an Ipe file
--
--------------------------------------------------------------------------------
module Ipe.Attributes2
  ( CommonAttributes(CommonAttributes), HasCommonAttributes(..)
  , SymbolAttributesF(SymbolAttributes), SymbolAttributes
  , HasStroke(..), HasFill(..), HasPen(..), HasSymbolSize(..)

  , PathAttributesF(PathAttributes), PathAttributes
  , HasDash(..), HasLineCap(..), HasLineJoin(..), HasFillRule(..)
  , HasArrow(..), HasRArrow(..), HasStrokeOpacity(..), HasOpacity(..)
  , HasTiling(..), HasGradient(..)

  , GroupAttributesF(GroupAttributes), GroupAttributes
  , HasClip(..)

  , TextAttributesF(TextAttributes), TextAttributes
  , HasTextSize(..), HasTextWidth(..), HasTextHeight(..), HasDepth(..)
  , HasHAlign(..), HasVAlign(..), HasStyle(..)

  , ImageAttributes

  , test
  ) where

import Data.Functor.Classes
import Ipe.Attributes.Types
import Control.Lens hiding (elements)
import Data.Default
import Data.Text (Text)
import HGeometry.Matrix
import Ipe.Value
import Text.Read (lexP, step, parens, prec, (+++)
                , Lexeme(Ident), readPrec, readListPrec, readListPrecDefault)
import GHC.Generics(Generic)
import Ipe.Types hiding (commonAttributes)
import Ipe.Color

--------------------------------------------------------------------------------


-- instance Read r => Read (Matrix n m r) where
--   readPrec = undefined  -- parens $ (prec app_prec $ do
--              --                             Ident "NoAttr" <- lexP
--              --                             pure NoAttr)
--              --      +++ (prec up_prec $ do
--              --                             Ident "Attr" <- lexP
--              --                             a <- step readPrec
--              --                             pure $ Attr a)
--     where
--       app_prec = 10
--       up_prec = 5
--   readListPrec = readListPrecDefault



--------------------------------------------------------------------------------

data CommonAttributes f r = CommonAttributes
  { _layer           :: f LayerName
  , _matrix          :: f (Matrix 3 3 r)
  , _pin             :: f PinType
  , _transformations :: f TransformationTypes
  } deriving (Generic)

deriving instance (Show1 f, Show r) => Show (CommonAttributes f r)
-- deriving instance (Read1 f, Read r) => Read (CommonAttributes f r)
deriving instance (Eq1 f, Eq r)     => Eq   (CommonAttributes f r)
-- deriving instance (Ord1 f, Ord r)   => Ord  (CommonAttributes f r)

instance Functor f => Functor (CommonAttributes f) where
  fmap f (CommonAttributes l m p t) = CommonAttributes l (fmap (fmap f) m) p t
instance Foldable f => Foldable (CommonAttributes f) where
  foldMap f (CommonAttributes _ m _ _) = foldMap (foldMap f) m
instance Traversable f => Traversable (CommonAttributes f) where
  traverse f (CommonAttributes l m p t) =
    (\m' -> CommonAttributes l m' p t) <$> traverse (traverse f) m

instance (forall a. Default (f a)) => Default (CommonAttributes f r) where
  def = CommonAttributes def def def def

--------------------------------------------------------------------------------

type SymbolAttributes = SymbolAttributesF Maybe

data SymbolAttributesF f r = SymbolAttributes
  { _commonAttrs :: !(CommonAttributes f r)
  , _stroke      :: f (IpeColor r)
  , _fill        :: f (IpeColor r)
  , _pen         :: f (IpeColor r)
  , _symbolSize  :: f (IpeSize r)
  } deriving (Generic)

deriving instance (Show1 f, Show r) => Show (SymbolAttributesF f r)
-- deriving instance (Read1 f, Read r) => Read (SymbolAttributesF f r)
deriving instance (Eq1 f, Eq r)     => Eq   (SymbolAttributesF f r)
-- deriving instance (Ord1 f, Ord r)   => Ord  (SymbolAttributesF f r)

instance Functor f => Functor (SymbolAttributesF f) where
  fmap f (SymbolAttributes com s fi p si) =
    SymbolAttributes (fmap f com)
                     (fmap (fmap f) s)
                     (fmap (fmap f) fi)
                     (fmap (fmap f) p)
                     (fmap (fmap f) si)

instance Foldable f => Foldable (SymbolAttributesF f) where
  foldMap f (SymbolAttributes com s fi p si) = foldMap f com
                                            <> foldMap (foldMap f) s
                                            <> foldMap (foldMap f) fi
                                            <> foldMap (foldMap f) p
                                            <> foldMap (foldMap f) si

instance Traversable f => Traversable (SymbolAttributesF f) where
  traverse f (SymbolAttributes com s fi p si) =
    SymbolAttributes <$> traverse f com
                     <*> traverse (traverse f) s
                     <*> traverse (traverse f) fi
                     <*> traverse (traverse f) p
                     <*> traverse (traverse f) si


instance (forall a. Default (f a)) => Default (SymbolAttributesF f r) where
  def = SymbolAttributes def def def def def

--------------------------------------------------------------------------------

type PathAttributes   = PathAttributesF Maybe

-- | Path Attributes
data PathAttributesF f r = PathAttributes
  { _commonAttrs   :: !(CommonAttributes f r)
  , _stroke        :: f (IpeColor r)
  , _fill          :: f (IpeColor r)
  , _pen           :: f (IpeColor r)
  , _dash          :: f (IpeDash r)
  , _lineCap       :: f Int
  , _lineJoin      :: f Int
  , _fillRule      :: f (IpeArrow r)
  , _arrow         :: f (IpeArrow r)
  , _rArrow        :: f (IpeArrow r)
  , _strokeOpacity :: f (IpeValue r)
  , _opacity       :: f (IpeValue r)
  , _tiling        :: f IpeTiling
  , _gradient      :: f IpeGradient
  } deriving (Generic)


deriving instance (Show1 f, Show r) => Show (PathAttributesF f r)
-- deriving instance (Read1 f, Read r) => Read (PathAttributesF f r)
deriving instance (Eq1 f, Eq r)     => Eq   (PathAttributesF f r)
-- deriving instance (Ord1 f, Ord r)   => Ord  (PathAttributesF f r)


instance (forall a. Default (f a)) => Default (PathAttributesF f r) where
  def = PathAttributes def def def def def def def def def def def def def def

--------------------------------------------------------------------------------

type TextAttributes   = TextAttributesF Maybe

data TextAttributesF f r = TextAttributes
  { _commonAttrs :: !(CommonAttributes f r)
  , _stroke      :: f (IpeColor r)
  , _textSize    :: f (IpeSize r)
  , _opacity     :: f (IpeValue r)
  , _textWidth   :: f (TextSizeUnit r)
  , _textHeight  :: f (TextSizeUnit r)
  , _depth       :: f (TextSizeUnit r)
  , _hAlign      :: f HorizontalAlignment
  , _vAlign      :: f VerticalAlignment
  , _style       :: f TeXStyle
  } deriving (Generic)


deriving instance (Show1 f, Show r) => Show (TextAttributesF f r)
-- deriving instance (Read1 f, Read r) => Read (TextAttributesF f r)
deriving instance (Eq1 f, Eq r)     => Eq   (TextAttributesF f r)
-- deriving instance (Ord1 f, Ord r)   => Ord  (TextAttributesF f r)

instance (forall a. Default (f a)) => Default (TextAttributesF f r) where
  def = TextAttributes def def def def def def def def def def

--------------------------------------------------------------------------------

type GroupAttributes  = GroupAttributesF Maybe

data GroupAttributesF f r = GroupAttributes
  { _commonAttrs :: !(CommonAttributes f r)
  , _clip        :: f () -- FIXME!!
  } deriving (Generic)


deriving instance (Show1 f, Show r) => Show (GroupAttributesF f r)
-- deriving instance (Read1 f, Read r) => Read (GroupAttributesF f r)
deriving instance (Eq1 f, Eq r)     => Eq   (GroupAttributesF f r)
-- deriving instance (Ord1 f, Ord r)   => Ord  (GroupAttributesF f r)

instance (forall a. Default (f a)) => Default (GroupAttributesF f r) where
  def = GroupAttributes def def

--------------------------------------------------------------------------------

makeClassy ''CommonAttributes

makeFieldsNoPrefix ''SymbolAttributesF
makeFieldsNoPrefix ''PathAttributesF
makeFieldsNoPrefix ''TextAttributesF
makeFieldsNoPrefix ''GroupAttributesF

-- TODO: maybe I want the common attributes to be fieldsNoPrefix after all
-- so that all fields are a class on their own

instance HasCommonAttributes (SymbolAttributesF f r) f r where
  commonAttributes = commonAttrs
instance HasCommonAttributes (PathAttributesF f r) f r where
  commonAttributes = commonAttrs
instance HasCommonAttributes (TextAttributesF f r) f r where
  commonAttributes = commonAttrs
instance HasCommonAttributes (GroupAttributesF f r) f r where
  commonAttributes = commonAttrs

--------------------------------------------------------------------------------

type ImageAttributes  = CommonAttributes

--------------------------------------------------------------------------------

class IpeWriteAttributes ats where
  -- | Write the attributes to pairs of texts
  ipeWriteAttrs :: ats -> [(Text,Text)]

instance IpeWriteAttributes (CommonAttributes Maybe r) where
  ipeWriteAttrs ats = mconcat . map (ats^.) $
                      [ layer.attrName "layer"
                      , matrix.attrName "matrix"
                      , pin.attrName "pin"
                      , transformations.attrName "transformations"
                      ]

instance IpeWriteAttributes (SymbolAttributes r) where
  ipeWriteAttrs ats = mconcat . map (ats^.) $
    [ commonAttrs.to ipeWriteAttrs
    , stroke.attrName "stroke"
    , fill.attrName "fill"
    , pen.attrName "pen"
    , symbolSize.attrName "size"
    ]

instance IpeWriteAttributes (PathAttributes r) where
  ipeWriteAttrs ats = mconcat . map (ats^.) $
    [ commonAttrs.to ipeWriteAttrs
    , stroke.attrName "stroke"
    , fill.attrName "fill"
    , pen.attrName "pen"
    , dash.attrName "dash"
    , lineCap.attrName "cap"
    , lineJoin.attrName "join"
    , fillRule.attrName "fillrule"
    , arrow.attrName "arrow"
    , rArrow.attrName "rarrow"
    , strokeOpacity.attrName "stroke-opacity"
    , opacity.attrName "opacity"
    , tiling.attrName "tiling"
    , gradient.attrName "gradient"
    ]

instance IpeWriteAttributes (TextAttributes r) where
  ipeWriteAttrs ats = mconcat . map (ats^.) $
    [ commonAttrs.to ipeWriteAttrs
    , stroke.attrName "stroke"
    , textSize.attrName "size"
    , opacity.attrName "opacity"
    , textWidth.attrName "width"
    , textHeight.attrName "height"
    , depth.attrName "depth"
    , hAlign.attrName "halign"
    , vAlign.attrName "valign"
    , style.attrName "style"
    ]

instance IpeWriteAttributes (GroupAttributes r) where
  ipeWriteAttrs ats = mconcat . map (ats^.) $
    [ commonAttrs.to ipeWriteAttrs
    , clip.attrName "clip"
    ]

attrName n = to $ mkAttr n ipeWriteText

mkAttr       :: Text -> (value -> Text) -> Maybe value -> [(Text,Text)]
mkAttr key f = maybe mempty ((:[]) . (key,) . f)


ipeWriteText = undefined

--------------------------------------------------------------------------------


test :: CommonAttributes Maybe Int
test = def&layer ?~ "foo"



--------------------------------------------------------------------------------




-- draw :: [lenses]
--      -> Path r -> [IpeObject r]
-- draw ats g = g :+
