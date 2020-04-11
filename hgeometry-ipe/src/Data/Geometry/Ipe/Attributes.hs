{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE UndecidableInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Geometry.Ipe.Attributes
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Possible Attributes we can assign to items in an Ipe file
--
--------------------------------------------------------------------------------
module Data.Geometry.Ipe.Attributes where

import Control.Lens hiding (rmap, Const)
import Data.Geometry.Ipe.Value
import Data.Singletons
import Data.Singletons.TH
import Data.Text (Text)
import Data.Vinyl
import Data.Vinyl.TypeLevel
import Data.Vinyl.Functor
import Text.Read (lexP, step, parens, prec, (+++)
                , Lexeme(Ident), readPrec, readListPrec, readListPrecDefault)

--------------------------------------------------------------------------------


data AttributeUniverse = -- common
                         Layer | Matrix | Pin | Transformations
                       -- symbol
                       | Stroke | Fill | Pen | Size
                       -- Path
                       | Dash | LineCap | LineJoin
                       | FillRule | Arrow | RArrow | Opacity | Tiling | Gradient
                       -- Group
                       | Clip
                       -- Extra
--                       | X Text
                       deriving (Show,Read,Eq)


genSingletons [ ''AttributeUniverse ]


type CommonAttributes = [ Layer, Matrix, Pin, Transformations ]


type TextLabelAttributes = CommonAttributes
type MiniPageAttributes  = CommonAttributes

type ImageAttributes     = CommonAttributes


type SymbolAttributes = CommonAttributes ++
                          [Stroke, Fill, Pen, Size]

type PathAttributes = CommonAttributes ++
                      [ Stroke, Fill, Dash, Pen, LineCap, LineJoin
                      , FillRule, Arrow, RArrow, Opacity, Tiling, Gradient
                      ]

type GroupAttributes = CommonAttributes ++ '[ 'Clip]


-- | Attr implements the mapping from labels to types as specified by the
-- (symbol representing) the type family 'f'
newtype Attr (f :: TyFun u * -> *) -- Symbol repr. the Type family mapping
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
  mappend = (<>)

newtype Attributes (f :: TyFun u * -> *) (ats :: [u]) = Attrs (Rec (Attr f) ats)

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
  a `mappend` b = a <> b

instance Semigroup (Attributes f ats) where
  (Attrs as) <> (Attrs bs) = Attrs $ zipRecsWith mappend as bs

traverseAttrs               :: Applicative h
                            => (forall label. Attr f label -> h (Attr g label))
                            -> Attributes f ats -> h (Attributes g ats)
traverseAttrs f (Attrs ats) = Attrs <$> rtraverse f ats



zipRecsWith                       :: (forall a. f a -> g a -> h a)
                                  -> Rec f as -> Rec g as -> Rec h as
zipRecsWith _ RNil      _         = RNil
zipRecsWith f (r :& rs) (s :& ss) = f r s :& zipRecsWith f rs ss



attrLens   :: forall at ats proxy f. (at ∈ ats)
           => proxy at -> Lens' (Attributes f ats) (Maybe (Apply f at))
attrLens _ = unAttrs.(rlens @at).getAttr

lookupAttr   :: (at ∈ ats) => proxy at -> Attributes f ats -> Maybe (Apply f at)
lookupAttr p = view (attrLens p)

setAttr               :: forall proxy at ats f. (at ∈ ats)
                      => proxy at -> Apply f at -> Attributes f ats -> Attributes f ats
setAttr _ a (Attrs r) = Attrs $ rput (Attr a :: Attr f at) r


-- | gets and removes the attribute from Attributes
takeAttr       :: forall proxy at ats f. (at ∈ ats)
               => proxy at -> Attributes f ats -> ( Maybe (Apply f at)
                                                  , Attributes f ats )
takeAttr p ats = (lookupAttr p ats, ats&attrLens p .~ Nothing)


-- | unsets/Removes an attribute
unSetAttr   :: forall proxy at ats f. (at ∈ ats)
            => proxy at -> Attributes f ats -> Attributes f ats
unSetAttr p = snd . takeAttr p


attr     :: (at ∈ ats, RecApplicative ats)
         => proxy at -> Apply f at -> Attributes f ats
attr p x = setAttr p x mempty




--------------------------------------------------------------------------------
-- | Common Attributes

-- IpeObjects may have attributes. Essentially attributes are (key,value)
-- pairs. The key is some name. Which attributes an object can have depends on
-- the type of the object. However, all ipe objects support the following
-- 'common attributes':

-- data CommonAttributeUniverse = Layer | Matrix | Pin | Transformations
--                              deriving (Show,Read,Eq)

-- | Possible values for Pin
data PinType = No | Yes | Horizontal | Vertical
             deriving (Eq,Show,Read)

-- | Possible values for Transformation
data TransformationTypes = Affine | Rigid | Translations deriving (Show,Read,Eq)

-- type family CommonAttrElf (r :: *) (f :: CommonAttributeUniverse)where
--   CommonAttrElf r 'Layer          = Text
--   CommonAttrElf r 'Matrix         = Matrix 3 3 r
--   CommonAttrElf r Pin             = PinType
--   CommonAttrElf r Transformations = TransformationTypes

-- genDefunSymbols [''CommonAttrElf]


-- type CommonAttributes r =
--   Attributes (CommonAttrElfSym1 r) [ 'Layer, 'Matrix, Pin, Transformations ]

--------------------------------------------------------------------------------
-- Text Attributes

-- these Attributes are speicifc to IpeObjects representing TextLabels and
-- MiniPages. The same structure as for the `CommonAttributes' applies here.

-- | TODO

--------------------------------------------------------------------------------
-- | Symbol Attributes

-- | The optional Attributes for a symbol
-- data SymbolAttributeUniverse = SymbolStroke | SymbolFill | SymbolPen | Size
--                              deriving (Show,Eq)

newtype IpeSize  r = IpeSize  (IpeValue r) deriving (Show,Eq,Ord,Functor,Foldable,Traversable)
newtype IpePen   r = IpePen   (IpeValue r) deriving (Show,Eq,Ord,Functor,Foldable,Traversable)


-- -- | And the corresponding types
-- type family SymbolAttrElf (r :: *) (s :: SymbolAttributeUniverse) :: * where
--   SymbolAttrElf r SymbolStroke = IpeColor
--   SymbolAttrElf r SymbolPen    = IpePen r
--   SymbolAttrElf r SymbolFill   = IpeColor
--   SymbolAttrElf r Size         = IpeSize r

-- genDefunSymbols [''SymbolAttrElf]


-- type SymbolAttributes r = [SymbolStroke, SymbolFill, SymbolPen, Size]

-- type SymbolAttributes r =
--   Attributes (SymbolAttrElfSym1 r) [SymbolStroke, SymbolFill, SymbolPen, Size]

-------------------------------------------------------------------------------
-- | Path Attributes

-- | Possible attributes for a path
-- data PathAttributeUniverse = Stroke | Fill | Dash | Pen | LineCap | LineJoin
--                            | FillRule | Arrow | RArrow | Opacity | Tiling | Gradient
--                            deriving (Show,Eq)


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

normalArrow :: IpeArrow r
normalArrow = IpeArrow "normal" (IpeSize $ Named "normal/normal")

--------------------------------------------------------------------------------
-- | Group Attributes

-- | The only group attribute is a Clip
-- data GroupAttributeUniverse = Clip deriving (Show,Read,Eq,Ord)

-- A clipping path is a Path. Which is defined in Data.Geometry.Ipe.Types. To
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
instance IpeAttrName Opacity    where attrName _ = "opacity"
instance IpeAttrName Tiling     where attrName _ = "tiling"
instance IpeAttrName Gradient   where attrName _ = "gradient"

-- GroupAttributeUniverse
instance IpeAttrName Clip     where attrName _ = "clip"


-- -- | Function that states that all elements in xs satisfy a given constraint c
-- type family AllSatisfy (c :: k -> Constraint) (xs :: [k]) :: Constraint where
--   AllSatisfy c '[] = ()
--   AllSatisfy c (x ': xs) = (c x, AllSatisfy c xs)


-- | Writing Attribute names
writeAttrNames           :: AllConstrained IpeAttrName rs => Rec f rs -> Rec (Const Text) rs
writeAttrNames RNil      = RNil
writeAttrNames (x :& xs) = Const (write'' x) :& writeAttrNames xs
  where
    write''   :: forall f s. IpeAttrName s => f s -> Text
    write'' _ = attrName (Proxy :: Proxy s)

--

--------------------------------------------------------------------------------
