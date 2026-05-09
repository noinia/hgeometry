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
module Ipe.Attributes
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


  , AttributeNames(..)
  , mkAttrs, applyAttrs
  , traverseCommon, traverseText, traversePath, traverseSymbol, traverseGroup
  ) where

import Data.Functor.Apply
import Data.Coerce
import Data.Kind (Type)
import Data.Functor.Classes
import Ipe.Attributes.Types
import Ipe.Path
import Control.Lens hiding (elements)
import Data.Default
import Data.Text (Text)
import HGeometry.Matrix
import Ipe.Value
import Text.Read (lexP, step, parens, prec, (+++)
                , Lexeme(Ident), readPrec, readListPrec, readListPrecDefault)
import GHC.Generics (Generic)
import Ipe.Color
import Barbies
import Barbies.Constraints (Dict(..))
import Ipe.Layer
import Ipe.Attributes.Types


--------------------------------------------------------------------------------

type ConversionError = String -- FIXME: remove

class IpeReadText t
instance IpeReadText r => IpeReadText (Path r)

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

instance (forall a. Semigroup (f a)) => Semigroup (CommonAttributes r f) where
  l <> r = bzipWith (<>) l r
instance ( (forall a. Monoid (f a))
         ) => Monoid (CommonAttributes r f) where
  mempty = bpure mempty

--------------------------------------------------------------------------------

type SymbolAttributes r = SymbolAttributesF r Maybe

data SymbolAttributesF r f = SymbolAttributes
  { _commonAttrs :: !(CommonAttributes r f)
  , _stroke      :: f (IpeColor r)
  , _fill        :: f (IpeColor r)
  , _pen         :: f (IpePen r)
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
  , _pen           :: f (IpePen r)
  , _dash          :: f (IpeDash r)
  , _lineCap       :: f Int
  , _lineJoin      :: f Int
  , _fillRule      :: f FillType
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
  , _clip        :: f (Path r)
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

-- | Type changing matrix lens
matrix' :: Lens (CommonAttributes r f) (CommonAttributes s f)
                (f (Matrix 3 3 r))     (f (Matrix 3 3 s))
matrix' f (CommonAttributes l m p t) = (\m' -> CommonAttributes l m' p t) <$> f m

--------------------------------------------------------------------------------

-- | Traverse for common attributes
traverseCommon       :: forall g f r s. (Applicative g, Traversable f)
                     => (r -> g s) -> CommonAttributes r f -> g (CommonAttributes s f)
traverseCommon g ats = ats&matrix' %%~ traverse g'
  where
    g'   :: Matrix 3 3 r -> g (Matrix 3 3 s)
    g' m = unwrapApplicative $ m&elements %%~ WrapApplicative . g

traverseSymbol       :: forall g f r s. (Applicative g, Traversable f)
                     => (r -> g s) -> SymbolAttributesF r f -> g (SymbolAttributesF s f)
traverseSymbol g (SymbolAttributes common s fi p si) =
  SymbolAttributes <$> traverseCommon g common
                   <*> traverse (traverse g) s
                   <*> traverse (traverse g) fi
                   <*> traverse (traverse g) p
                   <*> traverse (traverse g) si

traversePath       :: forall g f r s. (Applicative g, Traversable f)
                     => (r -> g s) -> PathAttributesF r f -> g (PathAttributesF s f)
traversePath g (PathAttributes common s f p d lc lj fr a ra so o t gr) =
  PathAttributes <$> traverseCommon g common
                 <*> traverse (traverse g) s
                 <*> traverse (traverse g) f
                 <*> traverse (traverse g) p
                 <*> traverse (traverse g) d
                 <*> pure lc
                 <*> pure lj
                 <*> pure fr
                 <*> traverse (traverse g) a
                 <*> traverse (traverse g) ra
                 <*> traverse (traverse g) so
                 <*> traverse (traverse g) o
                 <*> pure t
                 <*> pure gr

traverseText       :: forall g f r s. (Applicative g, Traversable f)
                     => (r -> g s) -> TextAttributesF r f -> g (TextAttributesF s f)
traverseText g (TextAttributes common s sz o w h d ha va st) =
  TextAttributes <$> traverseCommon g common
                 <*> traverse (traverse g) s
                 <*> traverse (traverse g) sz
                 <*> traverse (traverse g) o
                 <*> traverse (traverse g) w
                 <*> traverse (traverse g) h
                 <*> traverse (traverse g) d
                 <*> pure ha
                 <*> pure va
                 <*> pure st


  -- { _commonAttrs :: !(CommonAttributes r f)
  -- , _stroke      :: f (IpeColor r)
  -- , _textSize    :: f (IpeSize r)
  -- , _opacity     :: f (IpeValue r)
  -- , _textWidth   :: f (TextSizeUnit r)
  -- , _textHeight  :: f (TextSizeUnit r)
  -- , _depth       :: f (TextSizeUnit r)
  -- , _hAlign      :: f HorizontalAlignment
  -- , _vAlign      :: f VerticalAlignment
  -- , _style       :: f TeXStyle
  -- } deriving (Generic)




traverseGroup :: forall g f r s. (Applicative g, Traversable f)
              => (r -> g s) -> GroupAttributesF r f -> g (GroupAttributesF s f)
traverseGroup g (GroupAttributes common c) = GroupAttributes <$> traverseCommon g common
                                                           <*> traverse (traverse g) c

--------------------------------------------------------------------------------

class AttributeNames ats where
  -- | Construct the attribute names
  attributeNames :: ats (Const Text)

--------------------------------------------------------------------------------

instance AttributeNames (CommonAttributes r) where
  attributeNames = CommonAttributes
   { _layer           = Const "layer"
   , _matrix          = Const "matrix"
   , _pin             = Const "pin"
   , _transformations = Const "transformations"
   }


--------------------------------------------------------------------------------

instance AttributeNames (SymbolAttributesF r) where
  attributeNames = SymbolAttributes
    { _commonAttrs = attributeNames
    , _stroke      = Const "stroke"
    , _fill        = Const "fill"
    , _pen         = Const "pen"
    , _symbolSize  = Const "size"
    }

--------------------------------------------------------------------------------

instance AttributeNames (GroupAttributesF r) where
  attributeNames = GroupAttributes
    { _commonAttrs = attributeNames
    , _clip        = Const "clip"
    }


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

-- | Constructs attrs
mkAttrs :: Default at => [at -> at] -> at
mkAttrs = flip applyAttrs def

-- | Apply a the attributes
applyAttrs       :: [at -> at] -> at -> at
applyAttrs ats z = foldl' (flip ($)) z ats


-- type data Ipe r
-- type instance Rendered (Ipe r) = [Ipe.IpeObject r]


-- instance IsDrawable (Ipe r) (Path r) where
--   type AttrOf (Ipe r) (Path r) = PathAttributes r -> PathAttributes r
--   draw ats g = [ attrs ats
--                ]



-- red :: IpeColor Int
-- red = undefined



-- draw :: [lenses]
--      -> Path r -> [IpeObject r]
-- draw ats g = g :+



-- foo :: ( HasFill   g (Maybe (IpeColor Int))
--        , HasStroke g (Maybe (IpeColor Int))
--        ) => [Attr g]
-- foo = [ stroke ?~ red
--       , fill   ?~ green
--       ]

test :: CommonAttributes Int Maybe
test = def&layer ?~ "foo"
