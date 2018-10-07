{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Geometry.Ipe.Types where


import           Control.Lens
import           Data.Proxy
import           Data.Vinyl hiding (Label)

import           Data.Ext
import           Data.Geometry.Box(Rectangle)
import           Data.Geometry.Point
import           Data.Geometry.PolyLine
import           Data.Geometry.Polygon(SimplePolygon)
import           Data.Geometry.Properties
import           Data.Geometry.Transformation

import           Data.Maybe(mapMaybe)
import           Data.Singletons.TH(genDefunSymbols)

import           Data.Geometry.Ipe.Literal
import           Data.Geometry.Ipe.Color
import qualified Data.Geometry.Ipe.Attributes as AT
import           Data.Geometry.Ipe.Attributes hiding (Matrix)
import           Data.Text(Text)
import           Text.XML.Expat.Tree(Node)

import           GHC.Exts


import qualified Data.List.NonEmpty as NE
import qualified Data.LSeq          as LSeq

--------------------------------------------------------------------------------


newtype LayerName = LayerName {_layerName :: Text } deriving (Show,Read,Eq,Ord,IsString)

--------------------------------------------------------------------------------
-- | Image Objects


data Image r = Image { _imageData :: ()
                     , _rect      :: Rectangle () r
                     } deriving (Show,Eq,Ord)
makeLenses ''Image

type instance NumType   (Image r) = r
type instance Dimension (Image r) = 2

instance Fractional r => IsTransformable (Image r) where
  transformBy t = over rect (transformBy t)

--------------------------------------------------------------------------------
-- | Text Objects

data TextLabel r = Label Text (Point 2 r)
                 deriving (Show,Eq,Ord)

data MiniPage r = MiniPage Text (Point 2 r) r
                 deriving (Show,Eq,Ord)

type instance NumType   (TextLabel r) = r
type instance Dimension (TextLabel r) = 2

type instance NumType   (MiniPage r) = r
type instance Dimension (MiniPage r) = 2

instance Fractional r => IsTransformable (TextLabel r) where
  transformBy t (Label txt p) = Label txt (transformBy t p)

instance Fractional r => IsTransformable (MiniPage r) where
  transformBy t (MiniPage txt p w) = MiniPage txt (transformBy t p) w

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

type instance NumType   (IpeSymbol r) = r
type instance Dimension (IpeSymbol r) = 2

instance Fractional r => IsTransformable (IpeSymbol r) where
  transformBy t = over symbolPoint (transformBy t)



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

type instance NumType   (PathSegment r) = r
type instance Dimension (PathSegment r) = 2

instance Fractional r => IsTransformable (PathSegment r) where
  transformBy t (PolyLineSegment p) = PolyLineSegment $ transformBy t p
  transformBy t (PolygonPath p)     = PolygonPath $ transformBy t p
  transformBy _ _                   = error "transformBy: not implemented yet"


-- | A path is a non-empty sequence of PathSegments.
newtype Path r = Path { _pathSegments :: LSeq.LSeq 1 (PathSegment r) }
                 deriving (Show,Eq)
makeLenses ''Path

type instance NumType   (Path r) = r
type instance Dimension (Path r) = 2

instance Fractional r => IsTransformable (Path r) where
  transformBy t (Path s) = Path $ fmap (transformBy t) s

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
  AttrMap r AT.Matrix       = Matrix 3 3 r
  AttrMap r Pin             = PinType
  AttrMap r Transformations = TransformationTypes

  AttrMap r Stroke = IpeColor r
  AttrMap r Pen    = IpePen r
  AttrMap r Fill   = IpeColor r
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

type instance NumType   (Group r) = r
type instance Dimension (Group r) = 2

instance Fractional r => IsTransformable (Group r) where
  transformBy t (Group s) = Group $ fmap (transformBy t) s


type family AttributesOf (t :: * -> *) :: [u] where
  AttributesOf Group     = GroupAttributes
  AttributesOf Image     = CommonAttributes
  AttributesOf TextLabel = CommonAttributes
  AttributesOf MiniPage  = CommonAttributes
  AttributesOf IpeSymbol = SymbolAttributes
  AttributesOf Path      = PathAttributes


-- | Attributes' :: * -> [AttributeUniverse] -> *
type Attributes' r = Attributes (AttrMapSym1 r)

type IpeAttributes g r = Attributes' r (AttributesOf g)


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

type instance NumType   (IpeObject r) = r
type instance Dimension (IpeObject r) = 2

makePrisms ''IpeObject
makeLenses ''Group

class ToObject i where
  mkIpeObject :: IpeObject' i r -> IpeObject r

instance ToObject Group      where mkIpeObject = IpeGroup
instance ToObject Image      where mkIpeObject = IpeImage
instance ToObject TextLabel  where mkIpeObject = IpeTextLabel
instance ToObject MiniPage   where mkIpeObject = IpeMiniPage
instance ToObject IpeSymbol  where mkIpeObject = IpeUse
instance ToObject Path       where mkIpeObject = IpePath

instance Fractional r => IsTransformable (IpeObject r) where
  transformBy t (IpeGroup i)     = IpeGroup     $ i&core %~ transformBy t
  transformBy t (IpeImage i)     = IpeImage     $ i&core %~ transformBy t
  transformBy t (IpeTextLabel i) = IpeTextLabel $ i&core %~ transformBy t
  transformBy t (IpeMiniPage i)  = IpeMiniPage  $ i&core %~ transformBy t
  transformBy t (IpeUse i)       = IpeUse       $ i&core %~ transformBy t
  transformBy t (IpePath i)      = IpePath      $ i&core %~ transformBy t

-- | Shorthand for constructing ipeObjects
ipeObject'     :: ToObject i => i r -> IpeAttributes i r -> IpeObject r
ipeObject' i a = mkIpeObject $ i :+ a

commonAttributes :: Lens' (IpeObject r) (Attributes (AttrMapSym1 r) CommonAttributes)
commonAttributes = lens (Attrs . g) (\x (Attrs a) -> s x a)
  where
    select :: (CommonAttributes ⊆ AttributesOf g) =>
              Lens' (IpeObject' g r) (Rec (Attr (AttrMapSym1 r)) CommonAttributes)
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

-- | collect all non-group objects
flattenGroups :: [IpeObject r] -> [IpeObject r]
flattenGroups = concatMap flattenGroups'
  where
    flattenGroups'                              :: IpeObject r -> [IpeObject r]
    flattenGroups' (IpeGroup (Group gs :+ ats)) =
      map (applyAts ats) . concatMap flattenGroups' $ gs
        where
          applyAts _ = id
    flattenGroups' o                            = [o]

--------------------------------------------------------------------------------


-- | The definition of a view
-- make active layer into an index ?
data View = View { _layerNames      :: [LayerName]
                 , _activeLayer     :: LayerName
                 }
          deriving (Eq, Ord, Show)
makeLenses ''View

-- instance Default


-- | for now we pretty much ignore these
data IpeStyle = IpeStyle { _styleName :: Maybe Text
                         , _styleData :: Node Text Text
                         }
              deriving (Eq,Show)
makeLenses ''IpeStyle


basicIpeStyle :: IpeStyle
basicIpeStyle = IpeStyle (Just "basic") (xmlLiteral [litFile|resources/basic.isy|])


-- | The maybe string is the encoding
data IpePreamble  = IpePreamble { _encoding     :: Maybe Text
                                , _preambleData :: Text
                                }
                  deriving (Eq,Read,Show,Ord)
makeLenses ''IpePreamble

type IpeBitmap = Text



--------------------------------------------------------------------------------
-- Ipe Pages

-- | An IpePage is essentially a Group, together with a list of layers and a
-- list of views.
data IpePage r = IpePage { _layers  :: [LayerName]
                         , _views   :: [View]
                         , _content :: [IpeObject r]
                         }
              deriving (Eq,Show)
makeLenses ''IpePage

-- | Creates a simple page with no views.
fromContent     :: [IpeObject r] -> IpePage r
fromContent obs = IpePage layers' [] obs
  where
    layers' = mapMaybe (^.commonAttributes.attrLens SLayer) obs

-- | A complete ipe file
data IpeFile r = IpeFile { _preamble :: Maybe IpePreamble
                         , _styles   :: [IpeStyle]
                         , _pages    :: NE.NonEmpty (IpePage r)
                         }
               deriving (Eq,Show)
makeLenses ''IpeFile

-- | Convenience function to construct an ipe file consisting of a single page.
singlePageFile   :: IpePage r -> IpeFile r
singlePageFile p = IpeFile Nothing [basicIpeStyle] (p NE.:| [])

-- | Create a single page ipe file from a list of IpeObjects
singlePageFromContent :: [IpeObject r] -> IpeFile r
singlePageFromContent = singlePageFile . fromContent


--------------------------------------------------------------------------------

-- | Takes and applies the ipe Matrix attribute of this item.
applyMatrix'              :: ( IsTransformable (i r)
                             , AT.Matrix ∈ AttributesOf i
                             , Dimension (i r) ~ 2, r ~ NumType (i r))
                          => IpeObject' i r -> IpeObject' i r
applyMatrix' o@(i :+ ats) = maybe o (\m -> transformBy (Transformation m) i :+ ats') mm
  where
    (mm,ats') = takeAttr (Proxy :: Proxy AT.Matrix) ats

-- | Applies the matrix to an ipe object if it has one.
applyMatrix                  :: Fractional r => IpeObject r -> IpeObject r
applyMatrix (IpeGroup i)     = IpeGroup . applyMatrix'
                             $ i&core.groupItems.traverse %~ applyMatrix
                             -- note that for a group we first (recursively)
                             -- apply the matrices, and then apply
                             -- the matrix of the group to its members.
applyMatrix (IpeImage i)     = IpeImage     $ applyMatrix' i
applyMatrix (IpeTextLabel i) = IpeTextLabel $ applyMatrix' i
applyMatrix (IpeMiniPage i)  = IpeMiniPage  $ applyMatrix' i
applyMatrix (IpeUse i)       = IpeUse       $ applyMatrix' i
applyMatrix (IpePath i)      = IpePath      $ applyMatrix' i

applyMatrices   :: Fractional r => IpeFile r -> IpeFile r
applyMatrices f = f&pages.traverse %~ applyMatricesPage

applyMatricesPage   :: Fractional r => IpePage r -> IpePage r
applyMatricesPage p = p&content.traverse %~ applyMatrix


--------------------------------------------------------------------------------


-- -- | Access a path as if it was a PolyLine
-- _PolyLine :: Prism' (IpeObject' Path r)
--                     (PolyLine 2 () r :+ IpeAttributes Path r)
-- _PolyLine = prism' build' access
--   where
--     build'  p         = p&core %~ Path . S2.l1Singleton . PolyLineSegment
--     access ~(p :+ a) = (:+ a) <$> p^?pathSegments.S2.headL1._PolyLineSegment

-- -- | Access a path as if it was a SimplePolygon
-- _SimplePolygon :: Prism' (IpeObject' Path r)
--                          (SimplePolygon () r :+ IpeAttributes Path r)
-- _SimplePolygon = prism' build' access
--   where
--     build'  p         = p&core %~ Path . S2.l1Singleton . PolygonPath
--     access ~(p :+ a) = (:+ a) <$> p^?pathSegments.S2.headL1._PolygonPath
