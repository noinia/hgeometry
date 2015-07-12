{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}

{-# LANGUAGE OverloadedStrings #-}
module Data.Geometry.Ipe.Types where

import           Control.Applicative
import           Control.Lens
import           Data.Monoid
import           Data.Proxy
import           Data.Vinyl
import           Linear.Affine((.-.), qdA)

import           Data.Ext
import           Data.Geometry.Ball
import           Data.Geometry.Box(Rectangle)
import           Data.Geometry.Line
import           Data.Geometry.Point
import           Data.Geometry.PolyLine
import           Data.Geometry.Properties
import           Data.Geometry.Transformation(Matrix)
import           Data.Singletons.TH
import           Data.Vinyl.TypeLevel
import           Frames.CoRec

import           Data.Geometry.Ipe.Attributes
import           Data.Text(Text)

import           GHC.Exts

import           GHC.TypeLits

import qualified Data.Sequence as S
import qualified Data.Seq2     as S2

--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- | Image Objects


data Image r = Image { _imageData :: ()
                     , _rect      :: Rectangle () r
                     } deriving (Show,Eq,Ord)
makeLenses ''Image

type instance NumType (Image r) = r

--------------------------------------------------------------------------------
-- | Text Objects

data TextLabel r = Label Text (Point 2 r)
                 deriving (Show,Eq,Ord)

data MiniPage r = MiniPage Text (Point 2 r) r
                 deriving (Show,Eq,Ord)

type instance NumType (TextLabel r) = r
type instance NumType (MiniPage r)  = r

width                  :: MiniPage t -> t
width (MiniPage _ _ w) = w

--------------------------------------------------------------------------------
-- | Ipe Symbols, i.e. Points

-- | A symbol (point) in ipe
data IpeSymbol r = Symbol { _symbolPoint :: Point 2 r
                          , _symbolName  :: Text
                          }
                 deriving (Show,Eq,Ord)
makeLenses ''IpeSymbol

type instance NumType (IpeSymbol r) = r


-- | Example of an IpeSymbol. I.e. A symbol that expresses that the size is 'large'
-- sizeSymbol :: Attributes (AttrMapSym1 r) (SymbolAttributes r)
-- sizeSymbol = attr SSize (IpeSize $ Named "large")

--------------------------------------------------------------------------------
-- | Paths

-- | Paths consist of Path Segments. PathSegments come in the following forms:
data PathSegment r = PolyLineSegment        (PolyLine 2 () r)
                     -- TODO
                   | PolygonPath
                   | CubicBezierSegment     -- (CubicBezier 2 r)
                   | QuadraticBezierSegment -- (QuadraticBezier 2 r)
                   | EllipseSegment (Matrix 3 3 r)
                   | ArcSegment
                   | SplineSegment          -- (Spline 2 r)
                   | ClosedSplineSegment    -- (ClosedSpline 2 r)
                   deriving (Show,Eq,Ord)
makePrisms ''PathSegment

-- | A path is a non-empty sequence of PathSegments.
newtype Path r = Path { _pathSegments :: S2.ViewL1 (PathSegment r) }
                 deriving (Show,Eq,Ord)
makeLenses ''Path


type instance NumType (Path r) = r


-- | type that represents a path in ipe.
data Operation r = MoveTo (Point 2 r)
                 | LineTo (Point 2 r)
                 | CurveTo (Point 2 r) (Point 2 r) (Point 2 r)
                 | QCurveTo (Point 2 r) (Point 2 r)
                 | Ellipse (Matrix 3 3 r)
                 | ArcTo (Matrix 3 3 r) (Point 2 r)
                 | Spline [Point 2 r]
                 | ClosedSpline [Point 2 r]
                 | ClosePath
                 deriving (Eq, Show)
makePrisms ''Operation

--------------------------------------------------------------------------------
-- * Attribute Mapping


-- | The mapping between the labels of the the attributes and the types of the
-- attributes with these labels. For example, the 'Matrix' label/attribute should
-- have a value of type 'Matrix 3 3 r'.
type family AttrMap (r :: *) (l :: AttributeUniverse) :: * where
  AttrMap r 'Layer          = Text
  AttrMap r 'Matrix         = Matrix 3 3 r
  AttrMap r Pin             = PinType
  AttrMap r Transformations = TransformationTypes

  AttrMap r Stroke = IpeColor
  AttrMap r Pen    = IpePen r
  AttrMap r Fill   = IpeColor
  AttrMap r Size   = IpeSize r

  AttrMap r Dash     = IpeDash r
  AttrMap r LineCap  = Int
  AttrMap r LineJoin = Int
  AttrMap r FillRule = FillType
  AttrMap r Arrow    = IpeArrow r
  AttrMap r RArrow   = IpeArrow r
  AttrMap r Opacity  = IpeOpacity
  AttrMap r Tiling   = IpeTiling
  AttrMap r Gradient = IpeGradient

  AttrMap r Clip = Path r -- strictly we event want this to be a closed path I guess

genDefunSymbols [''AttrMap]


--------------------------------------------------------------------------------
-- | Groups and Objects

--------------------------------------------------------------------------------
-- | Group Attributes

-- -- | Now that we know what a Path is we can define the Attributes of a Group.
-- type family GroupAttrElf (r :: *) (s :: GroupAttributeUniverse) :: * where
--   GroupAttrElf r Clip = Path r -- strictly we event want this to be a closed path I guess

-- genDefunSymbols [''GroupAttrElf]

-- type GroupAttributes r = Attributes (GroupAttrElfSym1 r) '[ 'Clip]


-- | A group is essentially a list of IpeObjects.
newtype Group r = Group { _groupItems :: [IpeObject r] }
                  deriving (Show,Eq)

type instance NumType (Group r) = r














-- type family F (t :: *) :: k where
--   F Int = (->)
--   F Bool = Int

-- data Uni1 = A | B
-- data Uni2 = C | D

-- type family D (t :: *) where
--   D Int  = Uni1
--   D Bool = Uni2

-- type family G (t :: *) :: (D t) where
--   G Int  = A
--   G Bool = C



-- --
-- type family IpeObjectSymbolF (t :: *) :: (TyFun u * -> *) where
--   IpeObjectSymbolF (Group     r) = GroupAttrElfSym1 r
--   IpeObjectSymbolF (Image     r) = CommonAttrElfSym1 r
--   IpeObjectSymbolF (TextLabel r) = CommonAttrElfSym1 r
--   IpeObjectSymbolF (MiniPage  r) = CommonAttrElfSym1 r
--   IpeObjectSymbolF (IpeSymbol r) = SymbolAttrElfSym1 r
--   IpeObjectSymbolF (Path      r) = PathAttrElfSym1 r

type family IpeObjectAttrF (r :: *) (t :: * -> *) :: [u] where
  IpeObjectAttrF r Group     = GroupAttributes  r
  IpeObjectAttrF r Image     = CommonAttributes r
  IpeObjectAttrF r TextLabel = CommonAttributes r
  IpeObjectAttrF r MiniPage  = CommonAttributes r
  IpeObjectAttrF r IpeSymbol = SymbolAttributes r
  IpeObjectAttrF r Path      = PathAttributes   r


-- type family IpeObjectAttrF1 (t :: *) (u :: *) :: [u] where
--   IpeObjectAttrF (Group     r) GroupAttributeUniverse = GroupAttributes  r
--   IpeObjectAttrF (Image     r) CommonAttributeUniverse = CommonAttributes r
--   IpeObjectAttrF (TextLabel r) CommonAttributeUniverse = CommonAttributes r
--   IpeObjectAttrF (MiniPage  r) CommonAttributeUniverse = CommonAttributes r
--   IpeObjectAttrF (IpeSymbol r) SymbolAttributeUniverse = SymbolAttributes r
--   IpeObjectAttrF (Path      r) PathAttributeUniverse   = PathAttributes   r



-- type family IpeObjectAttrF (t :: *) (f :: TyFun u * -> *) :: [u] where
--   IpeObjectAttrF (Group     r) (GroupAttrElfSym1 r)  = GroupAttributes  r
--   IpeObjectAttrF (Image     r) (CommonAttrElfSym1 r) = CommonAttributes r
--   IpeObjectAttrF (TextLabel r) (CommonAttrElfSym1 r) = CommonAttributes r
--   IpeObjectAttrF (MiniPage  r) (CommonAttrElfSym1 r) = CommonAttributes r
--   IpeObjectAttrF (IpeSymbol r) (SymbolAttrElfSym1 r) = SymbolAttributes r
--   IpeObjectAttrF (Path      r) (PathAttrElfSym1 r)   = PathAttributes   r



type IpeAttributes g r =
  Attributes (AttrMapSym1 r) (IpeObjectAttrF r g)


-- | An IpeObject' is essentially the oject ogether with its attributes
type IpeObject' g r = g r :+ IpeAttributes g r

data IpeObject r =
    IpeGroup     (IpeObject' Group     r)
  | IpeImage     (IpeObject' Image     r)
  | IpeTextLabel (IpeObject' TextLabel r)
  | IpeMiniPage  (IpeObject' MiniPage  r)
  | IpeUse       (IpeObject' IpeSymbol r)
  | IpePath      (IpeObject' Path      r)



-- data IpeObject r =
--     IpeGroup     (Group     r :+ Attributes (GroupAttrElfSym1 r)  (GroupAttributes     r))
--   | IpeImage     (Image     r :+ Attributes (CommonAttrElfSym1 r) (ImageAttributes     r))
--   | IpeTextLabel (TextLabel r :+ Attributes (CommonAttrElfSym1 r) (TextLabelAttributes r))
--   | IpeMiniPage  (MiniPage  r :+ Attributes (CommonAttrElfSym1 r) (MiniPageAttributes  r))
--   | IpeUse       (IpeSymbol r :+ Attributes (SymbolAttrElfSym1 r) (SymbolAttributes    r))
--   | IpePath      (Path      r :+ Attributes (PathAttrElfSym1 r)   (PathAttributes      r))

-- | To Express constraints
-- type All' c i = RecAll (Attr (AttrMapSym1 (NumType i))) (IpeObjectAttrF i) c

-- type All' c i r = RecAll (Attr (AttrMapSym1 r)) (IpeObjectAttrF r i) c

-- type All'

-- type AllIpeObject (c :: * -> Constraint) r =
--   ( c (Group r),    c (Image r),     c (TextLabel r)
--   , c (MiniPage r), c (IpeSymbol r), c (Path r)
--   )

deriving instance (Show r) => Show (IpeObject r)
deriving instance (Eq r)   => Eq   (IpeObject r)

type instance NumType (IpeObject r) = r

makePrisms ''IpeObject

class ToObject i where
  ipeObject' :: i r -> IpeAttributes i r -> IpeObject r

instance ToObject Group      where ipeObject' g a = IpeGroup     (g :+ a)
instance ToObject Image      where ipeObject' p a = IpeImage     (p :+ a)
instance ToObject TextLabel  where ipeObject' p a = IpeTextLabel (p :+ a)
instance ToObject MiniPage   where ipeObject' p a = IpeMiniPage  (p :+ a)
instance ToObject IpeSymbol  where ipeObject' s a = IpeUse       (s :+ a)
instance ToObject Path       where ipeObject' p a = IpePath      (p :+ a)


-- test' :: IpeAttributes Group Integer
-- test' = mempty

-- test'' :: Attributes ( Integer) (IpeObjectAttrF (Path Integer))
-- test'' = mempty

-- gr :: Group Integer
-- gr = Group []

-- toI = ipeObject' gr test'

-- testz :: IpeAttributes (Group Integer)
-- testz = Attrs $ (Attr path) :& RNil

-- path :: Path Integer
-- path = Path mempty

--------------------------------------------------------------------------------



type XmlTree = Text


newtype Layer = Layer {_layerName :: Text } deriving (Show,Read,Eq,Ord,IsString)


-- | The definition of a view
-- make active layer into an index ?
data View = View { _layerNames      :: [Layer]
                 , _activeLayer     :: Layer
                 }
          deriving (Eq, Ord, Show)
makeLenses ''View


-- | for now we pretty much ignore these
data IpeStyle = IpeStyle { _styleName :: Maybe Text
                         , _styleData :: XmlTree
                         }
              deriving (Eq,Show,Read,Ord)
makeLenses ''IpeStyle

-- | The maybe string is the encoding
data IpePreamble  = IpePreamble { _encoding     :: Maybe Text
                                , _preambleData :: XmlTree
                                }
                  deriving (Eq,Read,Show,Ord)
makeLenses ''IpePreamble

type IpeBitmap = XmlTree



--------------------------------------------------------------------------------
-- Ipe Pages







-- | An IpePage is essentially a Group, together with a list of layers and a
-- list of views.
data IpePage r = IpePage { _layers :: [Layer]
                         , _views  :: [View]
                         , _pages  :: Group r
                         }
              deriving (Eq,Show)
makeLenses ''IpePage

-- | A complete ipe file
data IpeFile r = IpeFile { _preamble :: Maybe IpePreamble
                         , _styles   :: [IpeStyle]
                         , _ipePages :: [IpePage r]
                         }
               deriving (Eq,Show)


makeLenses ''IpeFile
