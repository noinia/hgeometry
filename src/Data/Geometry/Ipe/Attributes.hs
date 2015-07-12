{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Geometry.Ipe.Attributes where

import           Control.Applicative hiding (Const)
import           Control.Lens hiding (rmap, Const)
import qualified Data.Foldable as F
import           Data.Geometry.Transformation(Matrix)
import           Data.Monoid
import           Data.Singletons
import           Data.Singletons.TH
import           Data.String(IsString(..))
import           Data.Text(Text)
import qualified Data.Traversable as T
import           Data.Vinyl
import           Data.Vinyl.Functor
import           Data.Vinyl.TypeLevel

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


type CommonAttributes r = [ 'Layer, 'Matrix, Pin, Transformations ]


type TextLabelAttributes r = CommonAttributes r
type MiniPageAttributes  r = CommonAttributes r

type ImageAttributes     r = CommonAttributes r


type SymbolAttributes r = CommonAttributes r ++
                          [Stroke, Fill, Pen, Size]

type PathAttributes r = CommonAttributes r ++
                        [ Stroke, Fill, Dash, Pen, LineCap, LineJoin
                        , FillRule, Arrow, RArrow, Opacity, Tiling, Gradient
                        ]

type GroupAttributes r = CommonAttributes r ++ '[ 'Clip]


-- | Attr implements the mapping from labels to types as specified by the
-- (symbol representing) the type family 'f'
newtype Attr (f :: TyFun u * -> *) -- Symbol repr. the Type family mapping
                                   -- Labels in universe u to concrete types
             (label :: k) = GAttr { unAttr :: Maybe (Apply f label) }
                          deriving (Show,Read,Eq,Ord)

pattern Attr x = GAttr (Just x)
pattern NoAttr = GAttr Nothing

-- | Give pref. to the *RIGHT*
instance Monoid (Attr f l) where
  mempty                 = NoAttr
  _ `mappend` b@(Attr x) = b
  a `mappend` _          = a


newtype Attributes (f :: TyFun u * -> *) (ats :: [u]) =
  Attrs { unAttrs :: Rec (Attr f) ats }


-- type All' c i = RecAll (Attr (IpeObjectSymbolF i)) (IpeObjectAttrF i) c

-- deriving instance All' Show atsShow (Attributes f ats)

deriving instance (RecAll (Attr f) ats Show) => Show (Attributes f ats)

instance (RecAll (Attr f) ats Eq)   => Eq   (Attributes f ats) where
  (Attrs a) == (Attrs b) = and . recordToList
                         . zipRecsWith (\x (Compose (Dict y)) -> Const $ x == y) a
                         . (reifyConstraint (Proxy :: Proxy Eq)) $ b

instance RecApplicative ats => Monoid (Attributes f ats) where
  mempty                          = Attrs $ rpure mempty
  (Attrs as) `mappend` (Attrs bs) = Attrs $ zipRecsWith mappend as bs

zipRecsWith                       :: (forall a. f a -> g a -> h a)
                                  -> Rec f as -> Rec g as -> Rec h as
zipRecsWith f RNil      _         = RNil
zipRecsWith f (r :& rs) (s :& ss) = f r s :& zipRecsWith f rs ss

getAttr   :: (at ∈ ats) => proxy at -> Attributes f ats -> Maybe (Apply f at)
getAttr p = unAttr . rget p . unAttrs

setAttr               :: forall proxy at ats f. (at ∈ ats)
                      => proxy at -> Apply f at -> Attributes f ats -> Attributes f ats
setAttr p a (Attrs r) = Attrs $ rput (Attr a :: Attr f at) r

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


-- | Many types either consist of a symbolc value, or a value of type v
data IpeValue v = Named Text | Valued v deriving (Show,Eq,Ord)

instance IsString (IpeValue v) where
  fromString = Named . fromString

type Colour = Text -- TODO: Make this a Colour.Colour

newtype IpeSize r = IpeSize  (IpeValue r)      deriving (Show,Eq,Ord)
newtype IpePen  r = IpePen   (IpeValue r)      deriving (Show,Eq,Ord)
newtype IpeColor  = IpeColor (IpeValue Colour) deriving (Show,Eq,Ord)


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
               deriving (Show,Eq)

-- | Allowed Fill types
data FillType = Wind | EOFill deriving (Show,Read,Eq)

-- | IpeOpacity, IpeTyling, and IpeGradient are all symbolic values
type IpeOpacity  = Text
type IpeTiling   = Text
type IpeGradient = Text

-- | Possible values for an ipe arrow
data IpeArrow r = IpeArrow { _arrowName :: Text
                           , _arrowSize :: IpeSize r
                           } deriving (Show,Eq)
makeLenses ''IpeArrow


-- -- | and their types
-- type family PathAttrElf (r :: *) (s :: PathAttributeUniverse) :: * where
--   PathAttrElf r Stroke   = IpeColor
--   PathAttrElf r Fill     = IpeColor
--   PathAttrElf r Dash     = IpeDash r
--   PathAttrElf r Pen      = IpePen r
--   PathAttrElf r LineCap  = Int
--   PathAttrElf r LineJoin = Int
--   PathAttrElf r FillRule = FillType
--   PathAttrElf r Arrow    = IpeArrow r
--   PathAttrElf r RArrow   = IpeArrow r
--   PathAttrElf r Opacity  = IpeOpacity
--   PathAttrElf r Tiling   = IpeTiling
--   PathAttrElf r Gradient = IpeGradient

-- genDefunSymbols [''PathAttrElf]

-- type PathAttributes r = [ Stroke, Fill, Dash, Pen, LineCap, LineJoin
--                         , FillRule, Arrow, RArrow, Opacity, Tiling, Gradient
--                         ]

-- type PathAttributes r =
--   Attributes (PathAttrElfSym1 r) [ Stroke, Fill, Dash, Pen, LineCap, LineJoin
--                                  , FillRule, Arrow, RArrow, Opacity, Tiling, Gradient
--                                  ]

--------------------------------------------------------------------------------
-- | Group Attributes


-- | The only group attribute is a Clip
-- data GroupAttributeUniverse = Clip deriving (Show,Read,Eq,Ord)

-- A clipping path is a Path. Which is defined in Data.Geometry.Ipe.Types. To
-- avoid circular imports, we define GroupAttrElf and GroupAttribute there.


--------------------------------------------------------------------------------
