{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
module Data.Geometry.Ipe.Types where


import           Control.Lens
import           Data.Vinyl

import           Data.Ext
import           Data.Geometry.Point
-- import Data.Geometry.Matrix
import           Data.Geometry.PolyLine



import           Data.Text(Text)

import           GHC.Exts

import           GHC.TypeLits

import qualified Data.Sequence as S

--------------------------------------------------------------------------------

type Matrix d m r = r

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
data PinType = No | Yes | Horizontal | Vertical
             deriving (Eq,Show,Read)

data TransformationTypes = Affine | Rigid | Translations deriving (Show,Read,Eq)


data CommonAttributes r = Layer | MatrixA | Pin | Transformations  deriving (Show,Read,Eq)


newtype CommonAttrs cs     = CommonAttrs    () deriving (Show,Eq,Functor)
newtype GroupAttrs gs      = GroupAttrs     () deriving (Show,Eq,Functor)
newtype TextLabelAttrs tls = TextLabelAttrs () deriving (Show,Eq,Functor)
newtype MiniPageAttrs mps  = MiniPageAttrs  () deriving (Show,Eq,Functor)
newtype SymbolAttrs ss     = SymbolAttrs    () deriving (Show,Eq,Functor)
newtype PathAttrs ps       = PathAttrs      () deriving (Show,Eq,Functor)

--------------------------------------------------------------------------------

data Rectangle r = Rectangle { _lowerLeft :: Point 2 r
                             , _upperRight :: Point 2 r
                             }
                 deriving (Show,Eq,Ord)
makeLenses ''Rectangle

data Image r = Image { _imageData :: ()
                     , _rect      :: Rectangle r
                     } deriving (Show,Eq,Ord)
makeLenses ''Image

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
makeLenses ''IpeSymbol

--------------------------------------------------------------------------------

data PathSegment r = PolyLineSegment        (PolyLine 2 r ())
                     -- TODO
                   | PolygonPath
                   | CubicBezierSegment     -- (CubicBezier 2 r)
                   | QuadraticBezierSegment -- (QuadraticBezier 2 r)
                   | EllipseSegment
                   | ArcSegment
                   | SplineSegment          -- (Spline 2 r)
                   | ClosedSplineSegment    -- (ClosedSpline 2 r)
                   deriving (Show,Eq,Ord)

newtype Path r = Path { _pathSegments :: S.Seq (PathSegment r) }
                 deriving (Show,Eq,Ord)
makeLenses ''Path


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

--------------------------------------------------------------------------------

data IpeObject gt gs is ts mps ss ps r =
    IpeGroup     (Group gt r  :+ Rec GroupAttrs     gs)
  | IpeImage     (Image r     :+ Rec CommonAttrs    is)
  | IpeTextLabel (TextLabel r :+ Rec TextLabelAttrs ts)
  | IpeMiniPage  (MiniPage r  :+ Rec MiniPageAttrs  mps)
  | IpeUse       (IpeSymbol r :+ Rec SymbolAttrs    ss)
  | IpePath      (Path r      :+ Rec PathAttrs      ps)
    -- deriving (Show,Eq)

-- deriving instance (Show (Group gt r), Show (Rec GroupAttrs gs)) =>
--                   Show (IpeObject gt gs is ts mps ss ps r)

--------------------------------------------------------------------------------

data GroupAttributes = Clip deriving (Show,Read,Eq,Ord)

type family GroupElF f where
  GroupElF Clip = ()

data Group gt r where
  GNil  ::                     Group '[] r
  GCons :: IpeObject gt gs is ts mps ss ps r
        -> Group gtt r ->      Group (T7 gt gs is ts mps ss ps ': gtt) r
        -- -> Group gtt r ->      Group ((Proxy gt,Proxy gs,Proxy is,Proxy ts,Proxy mps, Proxy ss,Proxy ps) ': gtt) r

-- instance Traversable (Group gt) where
--   traverse f RNil =

-- Applicative f => (a -> f b) -> t a -> f (t b)
-- ltraverse :: Applicative f => (forall g. g -> f b) -> Group gt r -> f (Group gt r)


-- type family ListAll (rs :: [k]) (f :: k -> Constraint) :: Constraint where
--   ListAll '[]       f = ()
--   ListAll (x ': xs) f = (f x, ListAll xs f)


-- -- deriving instance Eq   (Group gs r)

-- -- instance Show (Group '[] r) where
-- --   show _ = "Group []"

-- instance ListAll gs Show => Show (Group gs r) where
--   show xs =

--   show (o `GCons` gs) = let s = "Group ["
--                             n = length s
--                         in concat [s, ", ", drop n $ show gs]

-- deriving instance Show (Rec GroupAttrs gs) => Show (Group gs r)


-- | Poly kinded 7 tuple
data T7 (a :: ka) (b :: kb) (c :: kc) (d :: kd) (e :: ke) (f :: kf) (g :: kg) = T7
        deriving (Show,Read,Eq,Ord)

--------------------------------------------------------------------------------

-- | Represents a page in ipe
data IpePage gs r = IpePage { _layers :: [Layer]
                            , _views  :: [View]
                            , _pages  :: Group gs r
                            }
              -- deriving (Eq, Show)

makeLenses ''IpePage


data IpePages gs r where
  PNil  :: IpePages '[] r
  PCons :: IpePage gs r -> IpePages gss r -> IpePages (gs ': gss) r


-- | A complete ipe file
data IpeFile gs r = IpeFile { _preamble :: Maybe IpePreamble
                            , _styles   :: [IpeStyle]
                            , _ipePages :: IpePages gs r
                            }
                  -- deriving (Eq,Show)

makeLenses ''IpeFile
