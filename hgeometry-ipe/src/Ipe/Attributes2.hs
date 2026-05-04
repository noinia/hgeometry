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
import GHC.Generics (Generic)
import Ipe.Types hiding (commonAttributes)
import Ipe.Color
import Barbies
import Barbies.Constraints (Dict(..))

--------------------------------------------------------------------------------
type ConversionError = String -- FIXME: remove

class IpeWriteText t
class IpeReadText t
instance IpeWriteText LayerName
instance IpeWriteText PinType
instance IpeWriteText ()
instance IpeReadText ()
instance IpeWriteText r => IpeWriteText (Matrix 3 3 r)
instance IpeWriteText r => IpeWriteText (IpeColor r)
instance IpeWriteText r => IpeWriteText (IpeSize r)
instance IpeWriteText TransformationTypes

ipeWriteText :: IpeWriteText r => r -> Text
ipeWriteText = undefined

ipeReadText :: IpeReadText r => Text -> Either ConversionError r
ipeReadText = undefined
----------------------------------------

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

data CommonAttributes r f = CommonAttributes
  { _layer           :: f LayerName
  , _matrix          :: f (Matrix 3 3 r)
  , _pin             :: f PinType
  , _transformations :: f TransformationTypes
  } deriving (Generic)

instance FunctorB     (CommonAttributes r)
instance TraversableB (CommonAttributes r)
instance ApplicativeB (CommonAttributes r)
instance ConstraintsB (CommonAttributes r)

deriving instance (Show1 f, Show r) => Show (CommonAttributes r f)
deriving instance (Eq1 f, Eq r)     => Eq   (CommonAttributes r f)

instance (forall a. Default (f a)) => Default (CommonAttributes r f) where
  def = bpure def

--------------------------------------------------------------------------------

type SymbolAttributes r = SymbolAttributesF r Maybe

data SymbolAttributesF r f = SymbolAttributes
  { _commonAttrs :: !(CommonAttributes r f)
  , _stroke      :: f (IpeColor r)
  , _fill        :: f (IpeColor r)
  , _pen         :: f (IpeColor r)
  , _symbolSize  :: f (IpeSize r)
  } deriving (Generic)

instance FunctorB     (SymbolAttributesF r)
instance TraversableB (SymbolAttributesF r)
instance ApplicativeB (SymbolAttributesF r)
instance ConstraintsB (SymbolAttributesF r)

deriving instance (Show1 f, Show r) => Show (SymbolAttributesF r f)
-- deriving instance (Read1 f, Read r) => Read (SymbolAttributesF r f)
deriving instance (Eq1 f, Eq r)     => Eq   (SymbolAttributesF r f)
-- deriving instance (Ord1 f, Ord r)   => Ord  (SymbolAttributesF r f)

instance (forall a. Default (f a)) => Default (SymbolAttributesF r f) where
  def = bpure def

--------------------------------------------------------------------------------

type PathAttributes r = PathAttributesF r Maybe

-- | Path Attributes
data PathAttributesF r f = PathAttributes
  { _commonAttrs   :: !(CommonAttributes r f)
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

instance FunctorB     (PathAttributesF r)
instance TraversableB (PathAttributesF r)
instance ApplicativeB (PathAttributesF r)
instance ConstraintsB (PathAttributesF r)

deriving instance (Show1 f, Show r) => Show (PathAttributesF r f)
-- deriving instance (Read1 f, Read r) => Read (PathAttributesF r f)
deriving instance (Eq1 f, Eq r)     => Eq   (PathAttributesF r f)
-- deriving instance (Ord1 f, Ord r)   => Ord  (PathAttributesF r f)

instance (forall a. Default (f a)) => Default (PathAttributesF r f) where
  def = bpure def

--------------------------------------------------------------------------------

type TextAttributes r  = TextAttributesF r Maybe

data TextAttributesF r f = TextAttributes
  { _commonAttrs :: !(CommonAttributes r f)
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

instance FunctorB     (TextAttributesF r)
instance TraversableB (TextAttributesF r)
instance ApplicativeB (TextAttributesF r)
instance ConstraintsB (TextAttributesF r)

deriving instance (Show1 f, Show r) => Show (TextAttributesF r f)
deriving instance (Eq1 f, Eq r)     => Eq   (TextAttributesF r f)

instance (forall a. Default (f a)) => Default (TextAttributesF r f) where
  def = bpure def

--------------------------------------------------------------------------------

type GroupAttributes r = GroupAttributesF r Maybe

data GroupAttributesF r f = GroupAttributes
  { _commonAttrs :: !(CommonAttributes r f)
  , _clip        :: f () -- FIXME!!
  } deriving (Generic)

instance FunctorB     (GroupAttributesF r)
instance TraversableB (GroupAttributesF r)
instance ApplicativeB (GroupAttributesF r)
instance ConstraintsB (GroupAttributesF r)

deriving instance (Show1 f, Show r) => Show (GroupAttributesF r f)
deriving instance (Eq1 f, Eq r)     => Eq   (GroupAttributesF r f)

instance (forall a. Default (f a)) => Default (GroupAttributesF r f) where
  def = bpure def

--------------------------------------------------------------------------------

makeClassy ''CommonAttributes

makeFieldsNoPrefix ''SymbolAttributesF
makeFieldsNoPrefix ''PathAttributesF
makeFieldsNoPrefix ''TextAttributesF
makeFieldsNoPrefix ''GroupAttributesF


instance HasCommonAttributes (SymbolAttributesF r f) r f where
  commonAttributes = commonAttrs
instance HasCommonAttributes (PathAttributesF r f) r f where
  commonAttributes = commonAttrs
instance HasCommonAttributes (TextAttributesF r f) r f where
  commonAttributes = commonAttrs
instance HasCommonAttributes (GroupAttributesF r f) r f where
  commonAttributes = commonAttrs

--------------------------------------------------------------------------------

type ImageAttributes r = CommonAttributes r Maybe

--------------------------------------------------------------------------------

class AttributeNames ats where
  -- | Construct the attribute names
  attributeNames :: ats (Const Text)

class IpeWriteAttributes ats where
  -- | Write the attributes to pairs of texts
  ipeWriteAttrs :: ats -> [(Text,Text)]

class IpeReadAttributes ats where
  -- | Given the attributes in their text form, try to parse them
  ipeReadAttrs :: [(Text,Text)] -> Either ConversionError ats

--------------------------------------------------------------------------------

instance AttributeNames (CommonAttributes r) where
  attributeNames = CommonAttributes
   { _layer           = Const "layer"
   , _matrix          = Const "matrix"
   , _pin             = Const "pin"
   , _transformations = Const "transformations"
   }

instance ( AllB IpeWriteText (CommonAttributes r)
         ) => IpeWriteAttributes (CommonAttributes r Maybe) where
  ipeWriteAttrs = bfoldMap getConst . bzipWithC @IpeWriteText writeAttr attributeNames

writeAttr :: forall a. (IpeWriteText a) => Const Text a -> Maybe a -> Const [(Text,Text)] a
writeAttr (Const attr) = Const . \case
  Nothing  -> []
  Just val -> [(attr,ipeWriteText val)]

instance ( AllB IpeReadText (CommonAttributes r)
         ) => IpeReadAttributes (CommonAttributes r Maybe) where
  ipeReadAttrs textAts = btraverseC @IpeReadText (parseAttr textAts) attributeNames

-- | Parse some text attribute
parseAttr                      :: [(Text,Text)]
                               -> IpeReadText value
                               => Const Text value -> Either ConversionError (Maybe value)
parseAttr textAts (Const name) = traverse ipeReadText $ lookup name textAts
  -- case lookup name textAts of
  --                      Nothing  -> Right Nothing
  --                      Just txt -> Just <$> ipeReadText txt

--------------------------------------------------------------------------------

instance AttributeNames (SymbolAttributesF r) where
  attributeNames = SymbolAttributes
    { _commonAttrs = attributeNames
    , _stroke      = Const "stroke"
    , _fill        = Const "fill"
    , _pen         = Const "pen"
    , _symbolSize  = Const "size"
    }

instance ( AllB IpeWriteText (CommonAttributes r), IpeWriteText r
         ) => IpeWriteAttributes (SymbolAttributes r) where
  ipeWriteAttrs = ipeWriteAttrs'

instance ( AllB IpeReadText (SymbolAttributesF r), AllB IpeWriteText (CommonAttributes r)
         ) => IpeReadAttributes (SymbolAttributes r) where
  ipeReadAttrs = ipeReadAttrs'

-- | Implementation of ipeWriteAttrs for the various attribue types
ipeWriteAttrs'     :: ( AllB IpeWriteText b, HasCommonAttributes (b Maybe) r f
                      , IpeWriteAttributes (CommonAttributes r f), TraversableB b
                      , ConstraintsB b, ApplicativeB b, AttributeNames b
                      ) => b Maybe -> [(Text, Text)]
ipeWriteAttrs' ats = foldMapOf commonAttributes ipeWriteAttrs ats
                  <> bfoldMap getConst (bzipWithC @IpeWriteText writeAttr attributeNames ats)

-- | Implementation of ipeReadAttrs for the various attribue types
ipeReadAttrs'         :: (AllB IpeReadText b,
                          HasCommonAttributes (b Maybe) r f,
                          IpeReadAttributes (CommonAttributes r f), TraversableB b,
                          ConstraintsB b, AttributeNames b
                         ) => [(Text, Text)] -> Either ConversionError (b Maybe)
ipeReadAttrs' textAts = combine <$> ipeReadAttrs textAts
                                <*> btraverseC @IpeReadText (parseAttr textAts) attributeNames
  where
    combine common rest = rest&commonAttributes .~ common

--------------------------------------------------------------------------------

instance AttributeNames (GroupAttributesF r) where
  attributeNames = GroupAttributes
    { _commonAttrs = attributeNames
    , _clip        = Const "clip"
    }

instance ( AllB IpeWriteText (CommonAttributes r), IpeWriteText r
         ) => IpeWriteAttributes (GroupAttributes r) where
  ipeWriteAttrs = ipeWriteAttrs'

instance ( AllB IpeReadText (SymbolAttributesF r), AllB IpeWriteText (CommonAttributes r)
         ) => IpeReadAttributes (GroupAttributes r) where
  ipeReadAttrs = ipeReadAttrs'

--------------------------------------------------------------------------------

instance AttributeNames (PathAttributesF r) where
  attributeNames = PathAttributes
    { _commonAttrs = attributeNames
    , _stroke        = Const "stroke"
    , _fill          = Const "fill"
    , _pen           = Const "pen"
    , _dash          = Const "dash"
    , _lineCap       = Const "linecap"
    , _lineJoin      = Const "linejoin"
    , _fillRule      = Const "fillrule"
    , _arrow         = Const "arrow"
    , _rArrow        = Const "rarrow"
    , _strokeOpacity = Const "stroke-opacity"
    , _opacity       = Const "opacity"
    , _tiling        = Const "tiling"
    , _gradient      = Const "gradient"
    }

--------------------------------------------------------------------------------

instance AttributeNames (TextAttributesF r) where
  attributeNames = TextAttributes
    { _commonAttrs = attributeNames
    , _stroke      = Const "stroke"
    , _textSize    = Const "size"
    , _opacity     = Const "opacity"
    , _textWidth   = Const "width"
    , _textHeight  = Const "height"
    , _depth       = Const "depth"
    , _hAlign      = Const "halign"
    , _vAlign      = Const "valign"
    , _style       = Const "style"
    }

--------------------------------------------------------------------------------

-- red :: IpeColor Int
-- red = undefined



-- draw :: [lenses]
--      -> Path r -> [IpeObject r]
-- draw ats g = g :+


type Attr g = g -> g

foo :: ( HasFill   g (Maybe (IpeColor Int))
       , HasStroke g (Maybe (IpeColor Int))
       ) => [Attr g]
foo = [ stroke ?~ red
      , fill   ?~ green
      ]

test :: CommonAttributes Int Maybe
test = def&layer ?~ "foo"
