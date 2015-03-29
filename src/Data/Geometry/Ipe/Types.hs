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

import           Linear.Affine((.-.), qdA)

import           Data.Ext
import           Data.Geometry.Point
import           Data.Geometry.Ball
import           Data.Geometry.Properties
import           Data.Geometry.Transformation(Transformation(..), Matrix
                                             , transformBy, transformationMatrix
                                             , translation, uniformScaling, (|.|))
import           Data.Geometry.Box(Rectangle)
import           Data.Geometry.Line

import           Data.Geometry.Ipe.Attributes
import           Data.Text(Text)
import           Data.TypeLevel.Filter

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

--------------------------------------------------------------------------------
-- | Text Objects

data TextLabel r = Label Text (Point 2 r)
                 deriving (Show,Eq,Ord)

data MiniPage r = MiniPage Text (Point 2 r) r
                 deriving (Show,Eq,Ord)

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
sizeSymbol :: SymbolAttribute Int Size
sizeSymbol = SymbolAttribute . IpeSize $ Named "large"

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


fromCircle            :: Floating r => Circle r -> PathSegment r
fromCircle (Ball c r) = EllipseSegment m
  where
    m = translation (toVec c) |.| uniformScaling (sqrt r) ^. transformationMatrix
    -- m is the matrix s.t. if we apply m to the unit circle centered at the origin, we
    -- get the input circle.

fromEllipse :: (Num r, Ord r) => Operation r -> Maybe (Circle r)
fromEllipse (Ellipse m) | q `onBall` b = Just b
                        | otherwise    = Nothing
  where
    t = Transformation m
    c = transformBy t origin
    p = transformBy t (point2 1 0)
    q = transformBy t (point2 0 1)
    b = fromCenterAndPoint c p

--------------------------------------------------------------------------------
-- | Group Attributes

-- | Now that we know what a Path is we can define the Attributes of a Group.
type family GroupAttrElf (s :: GroupAttributeUniverse) (r :: *) :: * where
  GroupAttrElf Clip r = Path r -- strictly we event want this to be a closed path I guess

newtype GroupAttribute r s = GroupAttribute (GroupAttrElf s r)

--------------------------------------------------------------------------------
-- | Groups

-- | To define groups, we need some poly kinded, type-level only, 2-tuples.
data (a :: ka) :.: (b :: kb)

-- | An IpeGroup can store IpeObjects. We distinguish the following different
-- ipeObjects. The parameter t will cary additional information about the
-- particular object. In particular, we will use it to keep track of the
-- attributes each item has.
--
-- Note: We will use this type on the type-level only! In particular, we will
-- use it as a Label in a Vinyl Rec.
data IpeObjectType t = IpeGroup     t
                     | IpeImage     t
                     | IpeTextLabel t
                     | IpeMiniPage  t
                     | IpeUse       t
                     | IpePath      t
                     deriving (Show,Read,Eq)

-- | A group is essentially a hetrogenious list of IpeObjects. We represent a
-- group by means of a Vinyl Rec.
type Group gt r = Rec (IpeObject r) gt

type instance NumType (Group gt r) = r

-- | This type family links each 'IpeObjectType' to the type (in Haskell) that
-- we use to represent such an IpeObject. In principle we represent each object
-- by means of an Ext (:+), in which the 'core' is one of the previously seen types
-- (i.e. an Image, TextLabel, IpeSymbol, Path, etc), and the extra is a Vinyl record
-- storing the attributes.
--
-- The different ipe types use a different Universe to draw the labels form, to
-- make sure that i.e. a Path only has attributes applicable to a Path.
--
-- We also see the parameter of the IpeObjectType here: in all but the group case it is
-- a type level list that tells which attributes the object has. In case of a group it is a
-- poly-kinded pair (i.e. a `gt :.: gs`)  where the gt is a type level list that captures
-- *ALL* type information of the objects stored in this group, and the *gs* is a type level
-- list that specifies which attributes this group itself has.
type IpeObjectElF r (f :: IpeObjectType k) = IpeObjectValueElF r f :+ IpeObjectAttrElF r f

type family IpeObjectValueElF r (f :: IpeObjectType k) :: * where
  IpeObjectValueElF r (IpeGroup (gt :.: gs)) = Group gt r
  IpeObjectValueElF r (IpeImage is)          = Image r
  IpeObjectValueElF r (IpeTextLabel ts)      = TextLabel r
  IpeObjectValueElF r (IpeMiniPage mps)      = MiniPage r
  IpeObjectValueElF r (IpeUse  ss)           = IpeSymbol r
  IpeObjectValueElF r (IpePath ps)           = Path r


type family RevIpeObjectValueElF (t :: *) :: (k -> IpeObjectType k) where
  RevIpeObjectValueElF (Group gt r)   = IpeGroup
  RevIpeObjectValueElF (Image r)      = IpeImage
  RevIpeObjectValueElF (TextLabel r)  = IpeTextLabel
  RevIpeObjectValueElF (MiniPage r)   = IpeMiniPage
  RevIpeObjectValueElF (IpeSymbol r ) = IpeUse
  RevIpeObjectValueElF (Path r)       = IpePath




type family IpeObjectAttrElF r (f :: IpeObjectType k) :: * where
  IpeObjectAttrElF r (IpeGroup (gt :.: gs)) = Rec (GroupAttribute     r) gs
  IpeObjectAttrElF r (IpeImage is)          = Rec (CommonAttribute    r) is
  IpeObjectAttrElF r (IpeTextLabel ts)      = Rec (TextLabelAttribute r) ts
  IpeObjectAttrElF r (IpeMiniPage mps)      = Rec (MiniPageAttribute  r) mps
  IpeObjectAttrElF r (IpeUse  ss)           = Rec (SymbolAttribute    r) ss
  IpeObjectAttrElF r (IpePath ps)           = Rec (PathAttribute      r) ps



type family IpeObjectAttrFunctorElF (f :: IpeObjectType k) :: (* -> u -> *) where
  IpeObjectAttrFunctorElF (IpeGroup (gt :.: gs)) = GroupAttribute
  IpeObjectAttrFunctorElF (IpeImage is)          = CommonAttribute
  IpeObjectAttrFunctorElF (IpeTextLabel ts)      = TextLabelAttribute
  IpeObjectAttrFunctorElF (IpeMiniPage mps)      = MiniPageAttribute
  IpeObjectAttrFunctorElF (IpeUse  ss)           = SymbolAttribute
  IpeObjectAttrFunctorElF (IpePath ps)           = PathAttribute



-- TODO: Maybe split this TF into two TFS' one that determines the core type, the other
-- that gives the Attribute wrapper type
-- It would be nice if we could tell taht IpeObjecTELF was injective ...

-- | An ipe Object is then simply a thin wrapper around the IpeObjectELF type family.
newtype IpeObject r (fld :: IpeObjectType k) =
  IpeObject { _ipeObject :: IpeObjectElF r fld }

makeLenses ''IpeObject

type instance NumType (IpeObject r t) = r

--------------------------------------------------------------------------------


symb'' :: IpeObjectElF Int (IpeUse '[Size])
symb'' = Symbol origin "myLargesymbol"  :+ ( sizeSymbol :& RNil )

symb :: IpeObjectElF Int (IpeUse ('[] :: [SymbolAttributeUniverse]))
symb = Symbol origin "foo" :+ RNil

symb' :: IpeObject Int (IpeUse '[Size])
symb' = IpeObject symb''

gr :: Group '[IpeUse '[Size]] Int
gr = symb' :& RNil

grr :: IpeObjectElF Int (IpeGroup ('[IpeUse '[Size]]
                                   :.:
                                   ('[] :: [GroupAttributeUniverse])
                                  )
                        )
grr = gr :+ RNil


grrr :: IpeObject Int (IpeGroup ('[IpeUse '[Size]] :.:
                                      ('[] :: [GroupAttributeUniverse])
                                )
                      )
grrr = IpeObject grr


points' :: forall gt r. Group gt r -> [Point 2 r]
points' = fmap (^.ipeObject.core.symbolPoint) . filterRec'

filterRec' :: forall gt r fld. (fld ~ IpeUse '[Size]) =>
              Rec (IpeObject r) gt -> [IpeObject r fld]
filterRec' = undefined
-- filterRec' = filterRec (Proxy :: Proxy fld)





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
data IpePage gs r = IpePage { _layers :: [Layer]
                            , _views  :: [View]
                            , _pages  :: Group gs r
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
