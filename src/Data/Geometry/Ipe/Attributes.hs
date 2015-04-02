{-# LANGUAGE TemplateHaskell #-}
module Data.Geometry.Ipe.Attributes where

import           Control.Lens
import           Data.Text(Text)
import           Data.Geometry.Transformation(Matrix)
import           Data.Singletons
import           Data.Singletons.TH


--------------------------------------------------------------------------------
-- | Common Attributes

-- IpeObjects may have attributes. Essentially attributes are (key,value)
-- pairs. The key is some name. Which attributes an object can have depends on
-- the type of the object. However, all ipe objects support the following
-- 'common attributes':
data CommonAttributeUniverse = Layer | Matrix | Pin | Transformations
                             deriving (Show,Read,Eq)


-- | The CommonAttrElf family lists names of the the common attributes
-- (i.e. the keys in the key value pairs), and specifies the types that the
-- values should have.  For example, it specifies that 'Matrix' is a (common)
-- attribute, and that the values of this attribute are of type 'Matrix 3 3 r'.
type family CommonAttrElf (f :: CommonAttributeUniverse) (r :: *) where
  CommonAttrElf 'Layer          r = Text
  CommonAttrElf 'Matrix         r = Matrix 3 3 r
  CommonAttrElf Pin             r = PinType
  CommonAttrElf Transformations r = TransformationTypes

-- | A wrapper type around common attributes.
newtype CommonAttribute r s = CommonAttribute (CommonAttrElf s r)

-- | Possible values for Pin
data PinType = No | Yes | Horizontal | Vertical
             deriving (Eq,Show,Read)

-- | Possible values for Transformation
data TransformationTypes = Affine | Rigid | Translations deriving (Show,Read,Eq)

--------------------------------------------------------------------------------
-- Text Attributes

-- these Attributes are speicifc to IpeObjects representing TextLabels and
-- MiniPages. The same structure as for the `CommonAttributes' applies here.

-- | TODO
newtype TextLabelAttribute s r = TextLabelAttribute (CommonAttribute s r)
newtype MiniPageAttribute s r = MiniPageAttribute (CommonAttribute s r)


--------------------------------------------------------------------------------
-- | Symbol Attributes

-- | The optional Attributes for a symbol
data SymbolAttributeUniverse = SymbolStroke | SymbolFill | SymbolPen | Size
                             deriving (Show,Eq)

-- | And the corresponding types
type family SymbolAttrElf (s :: SymbolAttributeUniverse) (r :: *) :: * where
  SymbolAttrElf SymbolStroke r = IpeColor
  SymbolAttrElf SymbolPen    r = IpePen r
  SymbolAttrElf SymbolFill   r = IpeColor
  SymbolAttrElf Size         r = IpeSize r


-- | Wrapper around the possible SymbolAttributes
newtype SymbolAttribute r s = SymbolAttribute (SymbolAttrElf s r)


-- | Many types either consist of a symbolc value, or a value of type v
data IpeValue v = Named Text | Valued v deriving (Show,Eq,Ord)

type Colour = Text -- TODO: Make this a Colour.Colour

newtype IpeSize r = IpeSize  (IpeValue r)      deriving (Show,Eq,Ord)
newtype IpePen  r = IpePen   (IpeValue r)      deriving (Show,Eq,Ord)
newtype IpeColor  = IpeColor (IpeValue Colour) deriving (Show,Eq,Ord)

-------------------------------------------------------------------------------
-- | Path Attributes

-- | Possible attributes for a path
data PathAttributeUniverse = Stroke | Fill | Dash | Pen | LineCap | LineJoin
                           | FillRule | Arrow | RArrow | Opacity | Tiling | Gradient
                           deriving (Show,Eq)

-- | and their types
type family PathAttrElf (s :: PathAttributeUniverse) (r :: *) :: * where
  PathAttrElf Stroke   r = IpeColor
  PathAttrElf Fill     r = IpeColor
  PathAttrElf Dash     r = IpeDash r
  PathAttrElf Pen      r = IpePen r
  PathAttrElf LineCap  r = Int
  PathAttrElf LineJoin r = Int
  PathAttrElf FillRule r = FillType
  PathAttrElf Arrow    r = IpeArrow r
  PathAttrElf RArrow   r = IpeArrow r
  PathAttrElf Opacity  r = IpeOpacity
  PathAttrElf Tiling   r = IpeTiling
  PathAttrElf Gradient r = IpeGradient

-- | Wrapper type around possible PathAttributes
newtype PathAttribute r s = PathAttribute (PathAttrElf s r)

-- | Possible values for Dash
data IpeDash r = DashNamed Text
               | DashPattern [r] r

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

--------------------------------------------------------------------------------
-- | Group Attributes


-- | The only group attribute is a Clip
data GroupAttributeUniverse = Clip deriving (Show,Read,Eq,Ord)

-- A clipping path is a Path. Which is defined in Data.Geometry.Ipe.Types. To
-- avoid circular imports, we define GroupAttrElf and GroupAttribute there.


--------------------------------------------------------------------------------

-- | Generate singletons for all the Universes
genSingletons [ ''CommonAttributeUniverse
              , ''SymbolAttributeUniverse
              , ''PathAttributeUniverse
              , ''GroupAttributeUniverse
              ]
