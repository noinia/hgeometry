{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}

{-# LANGUAGE OverloadedStrings #-}
module Data.Geometry.Ipe.Types where

import           Control.Applicative
import           Control.Lens
import           Data.Proxy
import           Data.Vinyl

import           Data.Ext
import           Data.Geometry.Point
import           Data.Geometry.Transformation(Matrix)
import           Data.Geometry.Box(Rectangle)
import           Data.Geometry.PolyLine


import           Data.Text(Text)
import           Data.TypeLevel.Filter

import           GHC.Exts

import           GHC.TypeLits

import qualified Data.Sequence as S

--------------------------------------------------------------------------------


type XmlTree = Text


type Layer = Text


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

-- | Possible values for Pin
data PinType = No | Yes | Horizontal | Vertical
             deriving (Eq,Show,Read)

-- | Possible values for Transformation
data TransformationTypes = Affine | Rigid | Translations deriving (Show,Read,Eq)


-- | Some common attributes applicable for all ipe objects
data CommonAttributeUniverse = Layer | Matrix | Pin | Transformations
                             deriving (Show,Read,Eq)

type family CommonAttrElf (a :: CommonAttributeUniverse) (r :: *) where
  CommonAttrElf 'Layer          r = Text
  CommonAttrElf 'Matrix         r = Matrix 3 3 r
  CommonAttrElf Pin             r = PinType
  CommonAttrElf Transformations r = TransformationTypes


newtype CommonAttrs cs     = CommonAttrs    () deriving (Show,Eq,Functor)
newtype GroupAttrs gs      = GroupAttrs     () deriving (Show,Eq,Functor)
newtype TextLabelAttrs tls = TextLabelAttrs () deriving (Show,Eq,Functor)
newtype MiniPageAttrs mps  = MiniPageAttrs  () deriving (Show,Eq,Functor)
newtype PathAttrs ps       = PathAttrs      () deriving (Show,Eq,Functor)

newtype SymbolAttrs ps       = SymbolAttrs      () deriving (Show,Eq,Functor)


--------------------------------------------------------------------------------

data Image r = Image { _imageData :: ()
                     , _rect      :: Rectangle () r
                     } deriving (Show,Eq,Ord)
makeLenses ''Image

--------------------------------------------------------------------------------

data TextLabel r = Label Text (Point 2 r)
                 deriving (Show,Eq,Ord)

data MiniPage r = MiniPage Text (Point 2 r) r
                 deriving (Show,Eq,Ord)

width                  :: MiniPage t -> t
width (MiniPage _ _ w) = w

--------------------------------------------------------------------------------

-- | Many types either consist of a symbolc value, or a value of type v
data IpeValue v = Named Text | Valued v deriving (Show,Eq,Ord)

type Colour = Text -- TODO: Make this a Colour.Colour

newtype IpeSize r = IpeSize  (IpeValue r)      deriving (Show,Eq,Ord)
newtype IpePen  r = IpePen   (IpeValue r)      deriving (Show,Eq,Ord)
newtype IpeColor  = IpeColor (IpeValue Colour) deriving (Show,Eq,Ord)

-- | The optional Attributes for a symbol
data SymbolAttributeUniverse = SymbolStroke | SymbolFill | SymbolPen | Size
                             deriving (Show,Eq)


type family SymbolAttrElf (s :: SymbolAttributeUniverse) (r :: *) :: * where
  SymbolAttrElf SymbolStroke r = IpeColor
  SymbolAttrElf SymbolPen    r = IpePen r
  SymbolAttrElf SymbolFill   r = IpeColor
  SymbolAttrElf Size         r = IpeSize r


newtype SymbolAttributes r s = SymbolAttributes (SymbolAttrElf s r)


-- | A symbol (point) in ipe
data IpeSymbol r = Symbol { _symbolPoint :: Point 2 r
                          , _symbolName  :: Text
                          }
                 deriving (Show,Eq,Ord)
makeLenses ''IpeSymbol

--------------------------------------------------------------------------------

-- | Possible attributes for a path
data PathAttributeUniverse = Stroke | Fill | Dash | Pen | LineCap | LineJoin
                           | FillRule | Arrow | RArrow | Opacity | Tiling | Gradient
                           deriving (Show,Eq)

-- | Possible values for Dash
data IpeDash r = DashNamed Text
               | DashPattern [r] r

-- | Possible values for an ipe arrow
data IpeArrow r = IpeArrow { _arrowName :: Text
                           , _arrowSize :: IpeSize r
                           } deriving (Show,Eq)
makeLenses ''IpeArrow

-- | Allowed Fill types
data FillType = Wind | EOFill deriving (Show,Read,Eq)

-- | IpeOpacity, IpeTyling, and IpeGradient are all symbolic values
type IpeOpacity  = Text
type IpeTiling   = Text
type IpeGradient = Text

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


newtype PathAttributes r s = PathAttributes (PathAttrElf s r)



data PathSegment r = PolyLineSegment        (PolyLine 2 () r)
                     -- TODO
                   | PolygonPath
                   | CubicBezierSegment     -- (CubicBezier 2 r)
                   | QuadraticBezierSegment -- (QuadraticBezier 2 r)
                   | EllipseSegment
                   | ArcSegment
                   | SplineSegment          -- (Spline 2 r)
                   | ClosedSplineSegment    -- (ClosedSpline 2 r)
                   deriving (Show,Eq,Ord)
makePrisms ''PathSegment


newtype Path r = Path { _pathSegments :: S.Seq (PathSegment r) }
                 deriving (Show,Eq,Ord)
makeLenses ''Path


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

data T2 (a :: ka) (b :: kb)

data IpeObjectType t = IpeGroup     t
                     | IpeImage     t
                     | IpeTextLabel t
                     | IpeMiniPage  t
                     | IpeUse       t
                     | IpePath      t
                     deriving (Show,Read,Eq)

type family IpeObjectElF r (f :: IpeObjectType [k]) :: * where
  -- IpeObjectElF r (IpeGroup (T2 gt gs))    = Group gt r  :+ Rec GroupAttrs     gs
  IpeObjectElF r (IpeImage is)            = Image r     :+ Rec CommonAttrs    is
  IpeObjectElF r (IpeTextLabel ts)        = TextLabel r :+ Rec TextLabelAttrs ts
  IpeObjectElF r (IpeMiniPage mps)        = MiniPage r  :+ Rec MiniPageAttrs  mps
  -- IpeObjectElF r (IpeUse  ss)             = IpeSymbol r :+ Rec (SymbolAttrs r) ss
  IpeObjectElF r (IpeUse  ss)             = IpeSymbol r :+ Rec SymbolAttrs    ss
  IpeObjectElF r (IpePath ps)             = Path r      :+ Rec PathAttrs      ps

newtype IpeObject r (fld :: IpeObjectType [k]) =
  IpeObject { _ipeObject :: IpeObjectElF r fld }

makeLenses ''IpeObject

-- data IpeObject gt gs is ts mps ss ps r =
--     IpeGroup     (Group gt r  :+ Rec GroupAttrs     gs)
--   | IpeImage     (Image r     :+ Rec CommonAttrs    is)
--   | IpeTextLabel (TextLabel r :+ Rec TextLabelAttrs ts)
--   | IpeMiniPage  (MiniPage r  :+ Rec MiniPageAttrs  mps)
--   | IpeUse       (IpeSymbol r :+ Rec SymbolAttrs    ss)
--   | IpePath      (Path r      :+ Rec PathAttrs      ps)
    -- deriving (Show,Eq)

-- deriving instance (Show (Group gt r), Show (Rec GroupAttrs gs)) =>
--                   Show (IpeObject gt gs is ts mps ss ps r)

--------------------------------------------------------------------------------

-- | Poly kinded 7 tuple
-- data T7 (a :: ka) (b :: kb) (c :: kc) (d :: kd) (e :: ke) (f :: kf) (g :: kg) = T7
--         deriving (Show,Read,Eq,Ord)


data GroupAttributeUniverse = Clip deriving (Show,Read,Eq,Ord)

type family GroupAttrElf (s :: GroupAttributeUniverse) (r :: *) :: * where
  GroupAttrElf Clip r = Path r -- strictly we event want this to be a closed path I guess

newtype GroupAttributes r s = GroupAttributes (GroupAttrElf s r)

-- data Group gt r where
--   GNil  ::                     Group '[] r
--   GCons :: IpeObject gt gs is ts mps ss ps r
--         -> Group gtt r ->      Group (T7 gt gs is ts mps ss ps ': gtt) r

type Group gt r = Rec (IpeObject r) gt

symb :: IpeObjectElF Int (IpeUse '[])
symb = Symbol origin "foo" :+ RNil

symb' :: IpeObject Int (IpeUse '[])
symb' = IpeObject symb

gr :: Group '[IpeUse '[]] Int
gr = symb' :& RNil


points' :: forall gt r. Group gt r -> [Point 2 r]
points' = fmap (^.ipeObject.core.symbolPoint) . filterRec'

filterRec' :: forall gt r fld. (fld ~ IpeUse '[]) =>
              Rec (IpeObject r) gt -> [IpeObject r fld]
filterRec' = undefined
-- filterRec' = filterRec (Proxy :: Proxy fld)

--------------------------------------------------------------------------------

newtype Group' gs r = Group' (Group gs r)

-- | Represents a page in ipe
data IpePage gs r = IpePage { _layers :: [Layer]
                            , _views  :: [View]
                            , _pages  :: Group' gs r
                            }
              -- deriving (Eq, Show)
makeLenses ''IpePage

newtype Page r gs = Page { _unP :: Page gs r }

type IpePages gss r = Rec (Page r) gss



-- | A complete ipe file
data IpeFile gs r = IpeFile { _preamble :: Maybe IpePreamble
                            , _styles   :: [IpeStyle]
                            , _ipePages :: IpePages gs r
                            }
                  -- deriving (Eq,Show)

makeLenses ''IpeFile
