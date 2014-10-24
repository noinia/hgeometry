{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}
module Data.Geometry.Ipe.Types where

import Data.Vinyl

import Data.Ext
import Data.Geometry.Point
-- import Data.Geometry.Matrix
-- import Data.Geometry.PolyLine



import Data.Text(Text)

import GHC.TypeLits

import qualified Data.Sequence as S

--------------------------------------------------------------------------------

type PolyLine d r pe = ()
type Matrix d m r = r


-- | A complete ipe file
data IpeFile gs r = IpeFile { _preamble :: Maybe IpePreamble
                            , _styles   :: [IpeStyle]
                            , _ipePages :: IpePages gs r
                            }
                  -- deriving (Eq,Show)

data IpePages gs r where
  PNil  :: IpePages '[] r
  PCons :: IpePage gs r -> IpePages gss r -> IpePages (gs ': gss) r

-- deriving instance Eq   (IpePages gs r)
-- deriving instance Show (IpePages gs r)


type XmlTree = Text

-- for now we pretty much ignore these
-- | the maybe string is the styles name
data IpeStyle = IpeStyle { _styleName :: Maybe Text
                         , _styleData :: XmlTree
                         }
              -- deriving (Eq,Show)

-- | The maybe string is the encoding
data IpePreamble  = IpePreamble { _encoding     :: Maybe Text
                                , _preambleData :: XmlTree
                                }
                  deriving (Eq,Read,Show)

type IpeBitmap = XmlTree


-- | Represents the <page> tag.
data IpePage gs r = IpePage [Layer] [View] (Group gs r)
              -- deriving (Eq, Show)


type Layer = Text

-- | The definition of a view
-- make active layer into an index ?
data View = View { layerNames      :: [Layer]
                 , activeLayer     :: Layer
                 }
          deriving (Eq, Ord, Show)


--------------------------------------------------------------------------------
data PinType = No | Yes | Horizontal | Vertical
             deriving (Eq,Show,Read)

data TransformationTypes = Affine | Rigid | Translations deriving (Show,Read,Eq)


data CommonAttributes r = Layer | MatrixA | Pin | Transformations  deriving (Show,Read,Eq)


data IpeObject gt gs is ts mps ss ps r = IG ()
  --   IpeGroup     (Group gt r  :+ Rec GroupAttrs     gs)
  -- | IpeImage     (Image r     :+ Rec CommonAttrs    is)
  -- | IpeTextLabel (TextLabel r :+ Rec TextLabelAttrs ts)
  -- | IpeMiniPage  (MiniPage r  :+ Rec MiniPageAttrs  mps)
  -- | IpeUse       (IpeSymbol r :+ Rec SymbolAttrs    ss)
  -- | IpePath      (Path r      :+ Rec PathAttrs      ps)
    deriving (Show,Eq)

newtype CommonAttrs cs     = CommonAttrs    () deriving (Show,Eq,Functor)
newtype GroupAttrs gs      = GroupAttrs     () deriving (Show,Eq,Functor)
newtype TextLabelAttrs tls = TextLabelAttrs () deriving (Show,Eq,Functor)
newtype MiniPageAttrs mps  = MiniPageAttrs  () deriving (Show,Eq,Functor)
newtype SymbolAttrs ss     = SymbolAttrs    () deriving (Show,Eq,Functor)
newtype PathAttrs ps       = PathAttrs      () deriving (Show,Eq,Functor)

--------------------------------------------------------------------------------

data GroupAttributes = Clip

data Group gt r where
  GNil  ::                     Group '[] r
  GCons :: IpeObject gt gs is ts mps ss ps r
        -> Group gtt r ->      Group (T7 gt gs is ts mps ss ps ': gtt) r
        -- -> Group gtt r ->      Group ((Proxy gt,Proxy gs,Proxy is,Proxy ts,Proxy mps, Proxy ss,Proxy ps) ': gtt) r


-- deriving instance Eq   (Group gs r)
-- deriving instance Show (Group gs r)


-- | Poly kinded 7 tuple
data T7 (a :: ka) (b :: kb) (c :: kc) (d :: kd) (e :: ke) (f :: kf) (g :: kg)

--------------------------------------------------------------------------------

data Rectangle r = Rectangle { _lowerLeft :: Point 2 r
                             , _upperRight :: Point 2 r
                             }
                 deriving (Show,Eq,Ord)


data Image r = Image { _imageData :: ()
                     , _rect      :: Rectangle r
                     } deriving (Show,Eq,Ord)


--------------------------------------------------------------------------------

-- data TextAttributes =

-- data TextType = Label | MiniPage deriving (Show,Eq)

data TextLabel r = Label Text (Point 2 r)
                 deriving (Show,Eq,Ord)

data MiniPage r = MiniPage Text (Point 2 r) r
                 deriving (Show,Eq,Ord)

width (MiniPage _ _ w) = w


--------------------------------------------------------------------------------

data SymbolAttributes = SymbolName | Pos | Stroke | Fill | Pen | Size deriving (Show,Eq,Ord)

data IpeSymbol r = Symbol { _symbolPoint :: Point 2 r
                          , _symbolName  :: Text
                          }
                 deriving (Show,Eq,Ord)


--------------------------------------------------------------------------------



newtype Path r = Path { _pathSegments :: S.Seq (PathSegment r) }
                 deriving (Show,Eq,Ord)

data PathSegment r = PolyLineSegment        (PolyLine 2 r ())
                     -- TODO
                   | PolygonPath
                   | CubicBezierSegment     -- (CubicBezier 2 r)
                   | QuadraticBezierSegment -- (QuadraticBezier 2 r)
                   | EllipseSegment
                   | ArcSegment
                   | SplineSegment          -- (Spline 2 r)
                   | ClosedSplineSegment    -- (ClosedSpline 2 r)
                   deriving (Show,Eq)




-- | type that represents a path in ipe.
data Operation r = MoveTo (Point 2 r)
                 | LineTo (Point 2 r)
                 | CurveTo (Point 2 r) (Point 2 r) (Point 2 r)
                 | QCurveTo (Point 2 r) (Point 2 r)
                 | Ellipse (Matrix 3 2 r)
                 | ArcTo (Matrix 3 2 r) (Point 2 r)
                 | Spline [Point 2 r]
                 | ClosedSpline [Point 2 r]
                 | ClosePath
                 deriving (Eq, Show)
