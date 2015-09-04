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
import           Data.Geometry.Polygon(SimplePolygon)
import           Data.Geometry.Properties
import           Data.Geometry.Transformation(Matrix)
import           Data.Maybe(mapMaybe)
import           Data.Singletons.TH(genDefunSymbols)
import           Data.Vinyl.TypeLevel
import           Frames.CoRec

import           Data.Geometry.Ipe.Attributes
import           Data.Text(Text)

import           GHC.Exts

import           GHC.TypeLits

import qualified Data.Sequence as S
import qualified Data.Seq2     as S2

--------------------------------------------------------------------------------


newtype LayerName = LayerName {_layerName :: Text } deriving (Show,Read,Eq,Ord,IsString)

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
                   | PolygonPath            (SimplePolygon () r)
                     -- TODO
                   | CubicBezierSegment     -- (CubicBezier 2 r)
                   | QuadraticBezierSegment -- (QuadraticBezier 2 r)
                   | EllipseSegment (Matrix 3 3 r)
                   | ArcSegment
                   | SplineSegment          -- (Spline 2 r)
                   | ClosedSplineSegment    -- (ClosedSpline 2 r)
                   deriving (Show,Eq)
makePrisms ''PathSegment

-- | A path is a non-empty sequence of PathSegments.
newtype Path r = Path { _pathSegments :: S2.ViewL1 (PathSegment r) }
                 deriving (Show,Eq)
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
  AttrMap r 'Layer          = LayerName
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

type family IpeObjectAttrF (r :: *) (t :: * -> *) :: [u] where
  IpeObjectAttrF r Group     = GroupAttributes  r
  IpeObjectAttrF r Image     = CommonAttributes r
  IpeObjectAttrF r TextLabel = CommonAttributes r
  IpeObjectAttrF r MiniPage  = CommonAttributes r
  IpeObjectAttrF r IpeSymbol = SymbolAttributes r
  IpeObjectAttrF r Path      = PathAttributes   r


type IpeAttributes g r =
  Attributes (AttrMapSym1 r) (IpeObjectAttrF r g)


-- | An IpeObject' is essentially the oject ogether with its attributes
type IpeObject' g r = g r :+ IpeAttributes g r

attributes :: Lens' (IpeObject' g r) (IpeAttributes g r)
attributes = extra

data IpeObject r =
    IpeGroup     (IpeObject' Group     r)
  | IpeImage     (IpeObject' Image     r)
  | IpeTextLabel (IpeObject' TextLabel r)
  | IpeMiniPage  (IpeObject' MiniPage  r)
  | IpeUse       (IpeObject' IpeSymbol r)
  | IpePath      (IpeObject' Path      r)


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

commonAttributes :: Lens' (IpeObject r) (Attributes (AttrMapSym1 r) (CommonAttributes r))
commonAttributes = lens (Attrs . g) (\x (Attrs a) -> s x a)
  where
    select :: (CommonAttributes r ⊆ IpeObjectAttrF r g) =>
              Lens' (IpeObject' g r) (Rec (Attr (AttrMapSym1 r)) (CommonAttributes r))
    select = attributes.unAttrs.rsubset

    g (IpeGroup i)     = i^.select
    g (IpeImage i)     = i^.select
    g (IpeTextLabel i) = i^.select
    g (IpeMiniPage i)  = i^.select
    g (IpeUse i)       = i^.select
    g (IpePath i)      = i^.select

    s (IpeGroup i)     a = IpeGroup     $ i&select .~ a
    s (IpeImage i)     a = IpeImage     $ i&select .~ a
    s (IpeTextLabel i) a = IpeTextLabel $ i&select .~ a
    s (IpeMiniPage i)  a = IpeMiniPage  $ i&select .~ a
    s (IpeUse i)       a = IpeUse       $ i&select .~ a
    s (IpePath i)      a = IpePath      $ i&select .~ a

--------------------------------------------------------------------------------



type XmlTree = Text




-- | The definition of a view
-- make active layer into an index ?
data View = View { _layerNames      :: [LayerName]
                 , _activeLayer     :: LayerName
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


type PageContent r = [IpeObject r]


-- | An IpePage is essentially a Group, together with a list of layers and a
-- list of views.
data IpePage r = IpePage { _layers  :: [LayerName]
                         , _views   :: [View]
                         , _content :: PageContent r
                         }
              deriving (Eq,Show)
makeLenses ''IpePage

-- | Creates a simple page with no views.
fromContent     :: [IpeObject r] -> IpePage r
fromContent obs = IpePage layers [] obs
  where
    layers = mapMaybe (^.commonAttributes.attrLens SLayer) obs

-- | A complete ipe file
data IpeFile r = IpeFile { _preamble :: Maybe IpePreamble
                         , _styles   :: [IpeStyle]
                         , _pages    :: [IpePage r]
                         }
               deriving (Eq,Show)

singlePageFile   :: IpePage r -> IpeFile r
singlePageFile p = IpeFile Nothing [] [p]


singlePageFromContent = singlePageFile . fromContent

makeLenses ''IpeFile
