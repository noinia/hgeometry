{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE UndecidableInstances #-}
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
  -- ( AttributeUniverse(..)
  -- ,

  -- )
  where

import Control.Lens hiding (rmap, Const)
import Data.Kind (Type)
import Data.Singletons
import Data.Singletons.TH
import Data.Text (Text)
import Data.Vinyl
import Data.Vinyl.Functor
import Data.Vinyl.TypeLevel
import Ipe.Value
import Text.Read (lexP, step, parens, prec, (+++)
                , Lexeme(Ident), readPrec, readListPrec, readListPrecDefault)

--------------------------------------------------------------------------------

-- | The possible Attributes supported in Ipe. To use these
-- attributes, you'll likely need their Singletons's version which is
-- Prefixed by an 'S'. E.g. the 'Fill' attribute is represented by a
-- singleton 'SFill :: Sing Fill'.
data AttributeUniverse = -- common
                         Layer | Matrix | Pin | Transformations
                       -- symbol
                       | Stroke | Fill | Pen | Size
                       -- Path
                       | Dash | LineCap | LineJoin
                       | FillRule | Arrow | RArrow | StrokeOpacity | Opacity | Tiling | Gradient
                       -- Text (Label and Minipage)
                       | Width | Height | Depth | VAlign | HAlign | Style
                       -- Group
                       | Clip
                       -- Extra
--                       | X Text
                       deriving (Show,Read,Eq)


genSingletons [ ''AttributeUniverse ]

-- | IpeObjects may have attributes. Essentially attributes are
-- (key,value) pairs. The key is some name. Which attributes an object
-- can have depends on the type of the object. However, all ipe
-- objects support the Common Attributes
type CommonAttributes = [ Layer, Matrix, Pin, Transformations ]

-- | All attributes applicable to Text (TextLabels and Minipages)
type TextAttributes = CommonAttributes ++
                      [Stroke, Size, Width, Height, Depth, VAlign, HAlign, Style, Opacity]

-- | All attributes applicable to TextLabels
type TextLabelAttributes = TextAttributes
-- | All attributes applicable to Minipages
type MiniPageAttributes  = TextAttributes
-- | All attributes applicable to Images
type ImageAttributes     = CommonAttributes

-- | All attributes applicable to Symbols/Marks
type SymbolAttributes = CommonAttributes ++ [Stroke, Fill, Pen, Size]

-- | All attributes applicable to Paths
type PathAttributes = CommonAttributes ++
                      [ Stroke, Fill, Dash, Pen, LineCap, LineJoin
                      , FillRule, Arrow, RArrow, StrokeOpacity, Opacity, Tiling, Gradient
                      ]

-- | All attributes applicable to Groups
type GroupAttributes = CommonAttributes ++ '[ 'Clip]

--------------------------------------------------------------------------------
-- * A single attribute Attr

-- | Attr implements the mapping from labels to types as specified by the
-- (symbol representing) the type family 'f'
newtype Attr (f :: TyFun u Type -> Type) -- Symbol repr. the Type family mapping
                                         -- Labels in universe u to concrete types
             (label :: u) = GAttr { _getAttr :: Maybe (Apply f label) }


deriving instance Eq   (Apply f label) => Eq   (Attr f label)
deriving instance Ord  (Apply f label) => Ord  (Attr f label)

makeLenses ''Attr

-- | Constructor for constructing an Attr given an actual value.
pattern Attr   :: Apply f label -> Attr f label
pattern Attr x = GAttr (Just x)

-- | An Attribute that is not set
pattern NoAttr :: Attr f label
pattern NoAttr = GAttr Nothing
{-# COMPLETE NoAttr, Attr #-}

-- | Traverse an attribute.
traverseAttr   :: Applicative h => (Apply f label -> h (Apply g label))
               -> Attr f label -> h (Attr g label)
traverseAttr f = \case
  Attr x -> Attr <$> f x
  NoAttr -> pure NoAttr

-- | Traverse for the situation where the type is not actually parameterized.
pureAttr :: (Applicative h, Apply f a ~ Apply g a) => Attr f a -> h (Attr g a)
pureAttr = pure . \case
    Attr a -> Attr a
    NoAttr -> NoAttr


instance Show (Apply f label) => Show (Attr f label) where
  showsPrec d NoAttr   = showParen (d > app_prec) $ showString "NoAttr"
    where app_prec = 10
  showsPrec d (Attr a) = showParen (d > up_prec) $
                           showString "Attr " . showsPrec (up_prec+1) a
    where up_prec  = 5

instance Read (Apply f label) => Read (Attr f label) where
  readPrec = parens $ (prec app_prec $ do
                                         Ident "NoAttr" <- lexP
                                         pure NoAttr)
                  +++ (prec up_prec $ do
                                         Ident "Attr" <- lexP
                                         a <- step readPrec
                                         pure $ Attr a)
    where
      app_prec = 10
      up_prec = 5
  readListPrec = readListPrecDefault



-- | Give pref. to the *RIGHT*
instance Semigroup (Attr f l) where
  _ <> b@(Attr _) = b
  a <> _          = a

instance Monoid (Attr f l) where
  mempty  = NoAttr

--------------------------------------------------------------------------------
-- * Attributes

-- | A collection of Attributes.
newtype Attributes (f :: TyFun u Type -> Type) (ats :: [u]) = Attrs (Rec (Attr f) ats)

-- | Get a vinyl Record with Attrs
unAttrs :: Lens (Attributes f ats) (Attributes f' ats') (Rec (Attr f) ats) (Rec (Attr f') ats')
unAttrs = lens (\(Attrs r) -> r) (const Attrs)

deriving instance ( RMap ats, ReifyConstraint Show (Attr f) ats, RecordToList ats
                  , RecAll (Attr f) ats Show) => Show (Attributes f ats)
-- deriving instance (RecAll (Attr f) ats Read) => Read (Attributes f ats)

instance ( ReifyConstraint Eq (Attr f) ats, RecordToList ats
         , RecAll (Attr f) ats Eq)   => Eq   (Attributes f ats) where
  (Attrs a) == (Attrs b) = and . recordToList
                         . zipRecsWith (\x (Compose (Dict y)) -> Const $ x == y) a
                         . (reifyConstraint @Eq) $ b

instance RecApplicative ats => Monoid (Attributes f ats) where
  mempty        = Attrs $ rpure mempty

instance Semigroup (Attributes f ats) where
  (Attrs as) <> (Attrs bs) = Attrs $ zipRecsWith (<>) as bs

-- | Traverse implementation for Attrs
traverseAttrs               :: Applicative h
                            => (forall label. Attr f label -> h (Attr g label))
                            -> Attributes f ats -> h (Attributes g ats)
traverseAttrs f (Attrs ats) = Attrs <$> rtraverse f ats

-- | Zip two Recs with the given function.
zipRecsWith                       :: (forall a. f a -> g a -> h a)
                                  -> Rec f as -> Rec g as -> Rec h as
zipRecsWith _ RNil      _         = RNil
zipRecsWith f (r :& rs) (s :& ss) = f r s :& zipRecsWith f rs ss


----------------------------------------

-- | Lens into a specific attribute, if it is set.
ixAttr   :: forall at ats proxy f. (at ∈ ats)
         => proxy at -> Lens' (Attributes f ats) (Maybe (Apply f at))
ixAttr _ = unAttrs.(rlens @at).getAttr

-- | Prism into a particular attribute.
_Attr   :: forall at ats proxy f. (at ∈ ats, RecApplicative ats)
         => proxy at -> Prism' (Attributes f ats) (Apply f at)
_Attr a = prism' setA getA
  where
    setA x = setAttr a x mempty
    getA = lookupAttr a

-- | Looks up a particular attribute.
lookupAttr   :: (at ∈ ats) => proxy at -> Attributes f ats -> Maybe (Apply f at)
lookupAttr p = view (ixAttr p)

-- | Sets a particular attribute
setAttr               :: forall proxy at ats f. (at ∈ ats)
                      => proxy at -> Apply f at -> Attributes f ats -> Attributes f ats
setAttr _ a (Attrs r) = Attrs $ rput (Attr a :: Attr f at) r


-- | gets and removes the attribute from Attributes
takeAttr       :: forall proxy at ats f. (at ∈ ats)
               => proxy at -> Attributes f ats -> ( Maybe (Apply f at)
                                                  , Attributes f ats )
takeAttr p ats = (lookupAttr p ats, ats&ixAttr p .~ Nothing)

-- | unsets/Removes an attribute
unSetAttr   :: forall proxy at ats f. (at ∈ ats)
            => proxy at -> Attributes f ats -> Attributes f ats
unSetAttr p = snd . takeAttr p

-- | Creates a singleton attribute
attr     :: (at ∈ ats, RecApplicative ats)
         => proxy at -> Apply f at -> Attributes f ats
attr p x = x^.re (_Attr p)

--------------------------------------------------------------------------------
-- * Implementations for Common Attributes

-- | Possible values for Pin
data PinType = No | Yes | Horizontal | Vertical
             deriving (Eq,Show,Read,Enum)

-- | Possible values for Transformation
data TransformationTypes = Affine | Rigid | Translations deriving (Show,Read,Eq,Enum)

--------------------------------------------------------------------------------
-- * Text Attributes

-- these Attributes are speicifc to IpeObjects representing TextLabels
-- and MiniPages. The same structure as for the `CommonAttributes'
-- applies here.

data HorizontalAlignment = AlignLeft | AlignHCenter | AlignRight
                         deriving (Show,Read,Eq,Ord,Enum)

data VerticalAlignment = AlignTop | AlignVCenter | AlignBottom | AlignBaseline
                       deriving (Show,Read,Eq,Ord,Enum)

-- | Should be a symbolic name.
type TeXStyle = Text

-- | size of text in points
type TextSizeUnit r = r

--------------------------------------------------------------------------------
-- * Symbol Attributes

-- | The optional Attributes for a symbol
-- data SymbolAttributeUniverse = SymbolStroke | SymbolFill | SymbolPen | Size
--                              deriving (Show,Eq)

-- | Size
newtype IpeSize  r = IpeSize  (IpeValue r) deriving (Show,Eq,Ord,Functor,Foldable,Traversable)
-- | Pen/Thickness
newtype IpePen   r = IpePen   (IpeValue r) deriving (Show,Eq,Ord,Functor,Foldable,Traversable)

-------------------------------------------------------------------------------
-- * Path Attributes

-- | Possible values for Dash
data IpeDash r = DashNamed Text
               | DashPattern [r] r
               deriving (Show,Eq,Functor,Foldable,Traversable)

-- | Allowed Fill types
data FillType = Wind | EOFill deriving (Show,Read,Eq)

-- | IpeOpacity, IpeTyling, and IpeGradient are all symbolic values
type IpeOpacity  = Text
type IpeTiling   = Text
type IpeGradient = Text

-- | Possible values for an ipe arrow
data IpeArrow r = IpeArrow { _arrowName :: Text
                           , _arrowSize :: IpeSize r
                           } deriving (Show,Eq,Functor,Foldable,Traversable)
makeLenses ''IpeArrow

-- | A normal arrow
normalArrow :: IpeArrow r
normalArrow = IpeArrow "normal" (IpeSize $ Named "normal")

--------------------------------------------------------------------------------
-- * Group Attributes

-- | The only group attribute is a Clip

-- A clipping path is a Path. Which is defined in Ipe.Types. To
-- avoid circular imports, we define GroupAttrElf and GroupAttribute there.

--------------------------------------------------------------------------------
-- * Attribute names in Ipe


-- | For the types representing attribute values we can get the name/key to use
-- when serializing to ipe.
class IpeAttrName (a :: AttributeUniverse) where
  attrName :: proxy a -> Text

-- CommonAttributeUnivers
instance IpeAttrName Layer           where attrName _ = "layer"
instance IpeAttrName Matrix          where attrName _ = "matrix"
instance IpeAttrName Pin             where attrName _ = "pin"
instance IpeAttrName Transformations where attrName _ = "transformations"

-- IpeSymbolAttributeUniversre
instance IpeAttrName Stroke       where attrName _ = "stroke"
instance IpeAttrName Fill         where attrName _ = "fill"
instance IpeAttrName Pen          where attrName _ = "pen"
instance IpeAttrName Size         where attrName _ = "size"

-- PathAttributeUniverse
instance IpeAttrName Dash       where attrName _ = "dash"
instance IpeAttrName LineCap    where attrName _ = "cap"
instance IpeAttrName LineJoin   where attrName _ = "join"
instance IpeAttrName FillRule   where attrName _ = "fillrule"
instance IpeAttrName Arrow      where attrName _ = "arrow"
instance IpeAttrName RArrow     where attrName _ = "rarrow"
instance IpeAttrName StrokeOpacity where attrName _ = "stroke-opacity"
instance IpeAttrName Opacity    where attrName _ = "opacity"
instance IpeAttrName Tiling     where attrName _ = "tiling"
instance IpeAttrName Gradient   where attrName _ = "gradient"

-- TextAttibuteUniverse
instance IpeAttrName Width   where attrName _ = "width"
instance IpeAttrName Height  where attrName _ = "height"
instance IpeAttrName Depth   where attrName _ = "depth"
instance IpeAttrName VAlign  where attrName _ = "valign"
instance IpeAttrName HAlign  where attrName _ = "halign"
instance IpeAttrName Style   where attrName _ = "style"

-- GroupAttributeUniverse
instance IpeAttrName Clip     where attrName _ = "clip"

-- | Writing Attribute names
writeAttrNames           :: AllConstrained IpeAttrName rs => Rec f rs -> Rec (Const Text) rs
writeAttrNames RNil      = RNil
writeAttrNames (x :& xs) = Const (write'' x) :& writeAttrNames xs
  where
    write''   :: forall f s. IpeAttrName s => f s -> Text
    write'' _ = attrName (Proxy :: Proxy s)

--

--------------------------------------------------------------------------------
