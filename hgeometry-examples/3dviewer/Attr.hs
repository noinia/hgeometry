{-# LANGUAGE TypeData  #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Attr
  ( IsDrawable(..)
  , Rendered(..)

  , Attr(..)
  , foldrWithKey, singleton, (!?)


  , StrokeAttr(..)
  , LayerAttr(..)

  , Ipe
  ) where

import qualified Data.Dependent.Map as DMap
import           Data.Kind (Type)
import qualified Ipe
import           Control.Lens
import           Data.GADT.Compare
import           Data.GADT.Show
import           Data.Constraint.Extras
import           HGeometry.Matrix (Matrix)
import           HGeometry.Ext
import           Data.Constraint.Extras.TH (deriveArgDict)
import           Data.GADT.Compare.TH (deriveGCompare, deriveGEq)
import           Data.GADT.Show.TH (deriveGShow)
import           Data.Vinyl.Notation
import           Data.Vinyl
import           Data.Coerce

import           HGeometry.Point
import           HGeometry.Polygon
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------

-- | An Attribute is basically a dependent map
newtype Attr backend geom = Attr (DMap.DMap (AttrOf backend geom) Identity)

deriving newtype instance ( GCompare (AttrOf backend geom)) => Semigroup (Attr backend geom)
deriving newtype instance ( GCompare (AttrOf backend geom)) => Monoid    (Attr backend geom)
deriving newtype instance ( GEq      (AttrOf backend geom)
                          , Has' Eq (AttrOf backend geom) Identity
                          ) => Eq (Attr backend geom)
deriving newtype instance (GShow      (AttrOf backend geom)
                          , Has' Show (AttrOf backend geom) Identity
                          ) => Show (Attr backend geom)

-- | Fold over the attributes
foldrWithKey              :: (forall v. AttrOf backend geom v -> Identity v -> b -> b) -> b
                          -> Attr backend geom -> b
foldrWithKey f z (Attr m) = DMap.foldrWithKey f z m

-- | Create a singleton attribute
singleton     :: AttrOf backend geom val -> val -> Attr backend geom
singleton k v = Attr (DMap.singleton k (Identity v))

-- | Access a property
(!?)          :: (GCompare (AttrOf backend geom))
              => Attr backend geom -> AttrOf backend geom val -> Maybe val
(Attr m) !? k = runIdentity <$> DMap.lookup k m

-- | Map some attribute from one backend to the other
mapKeysWith              :: forall backend backend' geom geom'.
                            GCompare (AttrOf backend' geom')
                         => (forall v. AttrOf backend' geom'  v -> v -> v -> v)
                          -- ^ the attributes are: \key oldValue newValue -> result
                         -> (forall v. AttrOf backend geom v -> AttrOf backend' geom' v)
                         -> Attr backend geom -> Attr backend' geom'
mapKeysWith f g (Attr m) = Attr $ DMap.mapKeysWith f' g m
  where
    f' :: forall v. AttrOf backend' geom' v -> Identity v -> Identity v -> Identity v
    f' k (Identity old) (Identity new) = Identity $ f k old new


-- | Apply a mapping function to the kewys.
mapKeys :: GCompare (AttrOf backend' geom')
        => (forall v. AttrOf backend geom v -> AttrOf backend' geom' v)
        -> Attr backend geom -> Attr backend' geom'
mapKeys = mapKeysWith (\_k _old new -> new)

--------------------------------------------------------------------------------

  -- | The type of objects a backend renders
type family Rendered  backend :: Type

-- | A class that expresses that something is drawable using a particular backend
class ( Monoid (Rendered backend)
      , GCompare (AttrOf backend geom)
      -- , GShow    (AttrOf backend geom)
      ) => IsDrawable backend geom where

  -- | A GADT that expresses possible attributes for a particular object
  type AttrOf backend geom :: Type -> Type

  -- | Draw some objects
  draw :: [Attr backend geom] -> geom -> Rendered backend



--------------------------------------------------------------------------------

type data Ipe r

type instance Rendered (Ipe r) = [Ipe.IpeObject r]


--------------------------------------------------------------------------------

data CommonAttributes :: Type -> Type -> Type where
  Layer           :: CommonAttributes r Ipe.LayerName
  Matrix          :: CommonAttributes r (Matrix 3 3 r)
  Pin             :: CommonAttributes r Ipe.PinType
  Transformations :: CommonAttributes r Ipe.TransformationTypes

deriving instance Show (CommonAttributes r a)

deriveGEq ''CommonAttributes
deriveGCompare ''CommonAttributes
deriveGShow ''CommonAttributes

instance (c Ipe.LayerName, c (Matrix 3 3 r), c Ipe.PinType, c Ipe.TransformationTypes
         ) => Has c (CommonAttributes r) where
  has k r = case k of
    Layer           -> r
    Matrix          -> r
    Pin             -> r
    Transformations -> r

----------------------------------------

data PathAttributes :: Type -> Type -> Type where
  PathCommon    :: CommonAttributes r val -> PathAttributes r val
  PathStroke    ::                           PathAttributes r (Ipe.IpeColor r)
  PathFill      ::                           PathAttributes r (Ipe.IpeColor r)
  Dash          ::                           PathAttributes r (Ipe.IpeDash r)
  PathPen       ::                           PathAttributes r (Ipe.IpePen r)
  LineCap       ::                           PathAttributes r Int
  LineJoin      ::                           PathAttributes r Int
  FillRule      ::                           PathAttributes r Ipe.FillType
  Arrow         ::                           PathAttributes r (Ipe.IpeArrow r)
  RArrow        ::                           PathAttributes r (Ipe.IpeArrow r)
  StrokeOpacity ::                           PathAttributes r Ipe.IpeOpacity
  Opacity       ::                           PathAttributes r Ipe.IpeOpacity
  Tiling        ::                           PathAttributes r Ipe.IpeTiling
  Gradient      ::                           PathAttributes r Ipe.IpeGradient

deriving instance Show (CommonAttributes r a) => Show (PathAttributes r a)

deriveGEq ''PathAttributes
deriveGCompare ''PathAttributes
deriveGShow ''PathAttributes

----------------------------------------

data TextAttributes :: Type -> Type -> Type where
  TextCommon :: CommonAttributes r val -> TextAttributes r val
  TextStroke :: TextAttributes r (Ipe.IpeColor r)
  TextSize   :: TextAttributes r (Ipe.IpeSize r)
  Width      :: TextAttributes r (Ipe.TextSizeUnit r)
  Height     :: TextAttributes r (Ipe.TextSizeUnit r)
  Depth      :: TextAttributes r (Ipe.TextSizeUnit r)
  VAlign     :: TextAttributes r Ipe.VerticalAlignment
  HAlign     :: TextAttributes r Ipe.HorizontalAlignment
  Style      :: TextAttributes r Ipe.TeXStyle
  TextOpacity:: TextAttributes r Ipe.IpeOpacity

deriving instance Show (CommonAttributes r a) => Show (TextAttributes r a)


deriveGEq ''TextAttributes
deriveGCompare ''TextAttributes
deriveGShow ''TextAttributes

----------------------------------------

data SymbolAttributes :: Type -> Type -> Type where
  SymbolCommon :: CommonAttributes r val -> SymbolAttributes r val
  SymbolStroke ::                           SymbolAttributes r (Ipe.IpeColor r)
  SymbolFill   ::                           SymbolAttributes r (Ipe.IpeColor r)
  SymbolSize   ::                           SymbolAttributes r (Ipe.IpeSize r)
  SymbolPen    ::                           SymbolAttributes r (Ipe.IpePen r)

deriving instance Show (CommonAttributes r a) => Show (SymbolAttributes r a)


deriveGEq ''SymbolAttributes
deriveGCompare ''SymbolAttributes
deriveGShow ''SymbolAttributes

----------------------------------------

data GroupAttributes :: Type -> Type -> Type where
  GroupCommon  :: CommonAttributes r val -> GroupAttributes r val
  Clip         ::                           GroupAttributes r (Ipe.Path r)


deriving instance Show (CommonAttributes r a) => Show (GroupAttributes r a)

deriveGEq ''GroupAttributes
deriveGCompare ''GroupAttributes
deriveGShow ''GroupAttributes

--------------------------------------------------------------------------------
-- * Instances for basic Ipe types

instance ( GCompare (CommonAttributes r)
         ) => IsDrawable (Ipe r) (Ipe.IpeObject r) where
  type AttrOf (Ipe r) (Ipe.IpeObject r) = CommonAttributes r
  draw ats' o = (:[]) $ case o of
      Ipe.IpeGroup o'     -> Ipe.IpeGroup     (f o')
      Ipe.IpeImage o'     -> Ipe.IpeImage     (f o')
      Ipe.IpeTextLabel o' -> Ipe.IpeTextLabel (f o')
      Ipe.IpeMiniPage o'  -> Ipe.IpeMiniPage  (f o')
      Ipe.IpeUse o'       -> Ipe.IpeUse       (f o')
      Ipe.IpePath o'      -> Ipe.IpePath      (f o')
    where
      ats = mconcat ats'
      f    :: forall i. ( Ipe.Layer           ∈ Ipe.AttributesOf i
                        , Ipe.Matrix          ∈ Ipe.AttributesOf i
                        , Ipe.Pin             ∈ Ipe.AttributesOf i
                        , Ipe.Transformations ∈ Ipe.AttributesOf i
                        , RecApplicative (Ipe.AttributesOf i)
                        )
           => i r :+ Ipe.IpeAttributes i r -> i r :+ Ipe.IpeAttributes i r
      f o' = o'&Ipe.attributes %~ \z -> foldrWithKey (extendCommon @i) z ats

-- | Extend common
extendCommon                      :: forall i r v.
                                     ( Ipe.Layer           ∈ Ipe.AttributesOf i
                                     , Ipe.Matrix          ∈ Ipe.AttributesOf i
                                     , Ipe.Pin             ∈ Ipe.AttributesOf i
                                     , Ipe.Transformations ∈ Ipe.AttributesOf i
                                     , RecApplicative (Ipe.AttributesOf i)
                                     )
                                  => CommonAttributes r v
                                  -> Identity v
                                  -> Ipe.IpeAttributes i r -> Ipe.IpeAttributes i r
extendCommon k (Identity val) acc = case k of
            Layer           -> acc <> Ipe.attr Ipe.SLayer           val
            Matrix          -> acc <> Ipe.attr Ipe.SMatrix          val
            Pin             -> acc <> Ipe.attr Ipe.SPin             val
            Transformations -> acc <> Ipe.attr Ipe.STransformations val

----------------------------------------

instance ( GCompare (PathAttributes r)
         ) => IsDrawable (Ipe r) (Ipe.Path r) where
  type AttrOf (Ipe r) (Ipe.Path r) = PathAttributes r
  draw ats path = [ Ipe.IpePath $ extendPathAts ats (path :+ mempty)  ]

extendPathAts        :: forall backend geom r.
                        (AttrOf backend geom ~ PathAttributes r)
                     => [Attr backend geom]
                     -> Ipe.Path r :+ Ipe.IpeAttributes Ipe.Path r
                     -> Ipe.Path r :+ Ipe.IpeAttributes Ipe.Path r
extendPathAts ats o' = o'&Ipe.attributes %~ \z -> foldrWithKey extend z (mconcat ats)
  where
    extend k (Identity val) acc = case k of
      PathCommon k' -> extendCommon @Ipe.Path k' (Identity val) acc
      PathStroke    -> acc <> Ipe.attr Ipe.SStroke        val
      PathFill      -> acc <> Ipe.attr Ipe.SFill          val
      Dash          -> acc <> Ipe.attr Ipe.SDash          val
      PathPen       -> acc <> Ipe.attr Ipe.SPen           val
      LineCap       -> acc <> Ipe.attr Ipe.SLineCap       val
      LineJoin      -> acc <> Ipe.attr Ipe.SLineJoin      val
      FillRule      -> acc <> Ipe.attr Ipe.SFillRule      val
      Arrow         -> acc <> Ipe.attr Ipe.SArrow         val
      RArrow        -> acc <> Ipe.attr Ipe.SRArrow        val
      StrokeOpacity -> acc <> Ipe.attr Ipe.SStrokeOpacity val
      Opacity       -> acc <> Ipe.attr Ipe.SOpacity       val
      Tiling        -> acc <> Ipe.attr Ipe.STiling        val
      Gradient      -> acc <> Ipe.attr Ipe.SGradient      val

--   foldrWithKey extendPathAts z (mconcat ats)

-- '                      :: PathAttributes r v
--                                    -> Identity v
--                                    -> Ipe.IpeAttributes Ipe.Path r
--                                    -> Ipe.IpeAttributes Ipe.Path r

----------------------------------------

-- TODO: IpeImage

--

instance ( GCompare (TextAttributes r)
         ) => IsDrawable (Ipe r) (Ipe.TextLabel r) where
  type AttrOf (Ipe r) (Ipe.TextLabel r) = TextAttributes r
  draw ats lbl = [ Ipe.IpeTextLabel $ lbl :+ ats' ]
    where
      ats' :: Ipe.IpeAttributes Ipe.TextLabel r
      ats' = foldrWithKey extend' mempty (mconcat ats)
        where
          extend'                      :: TextAttributes r v
                                       -> Identity v
                                       -> Ipe.IpeAttributes Ipe.TextLabel r
                                       -> Ipe.IpeAttributes Ipe.TextLabel r
          extend' k (Identity val) acc = case k of
            TextCommon k' -> extendCommon @Ipe.TextLabel k' (Identity val) acc
            TextStroke    -> acc <> Ipe.attr Ipe.SStroke  val
            TextSize      -> acc <> Ipe.attr Ipe.SSize    val
            Width         -> acc <> Ipe.attr Ipe.SWidth   val
            Height        -> acc <> Ipe.attr Ipe.SHeight  val
            Depth         -> acc <> Ipe.attr Ipe.SDepth   val
            VAlign        -> acc <> Ipe.attr Ipe.SVAlign  val
            HAlign        -> acc <> Ipe.attr Ipe.SHAlign  val
            Style         -> acc <> Ipe.attr Ipe.SStyle   val
            TextOpacity   -> acc <> Ipe.attr Ipe.SOpacity val

----------------------------------------

instance ( GCompare (SymbolAttributes r)
         ) => IsDrawable (Ipe r) (Ipe.IpeSymbol r) where
  type AttrOf (Ipe r) (Ipe.IpeSymbol r) = SymbolAttributes r
  draw ats sym = [ Ipe.IpeUse $ extendSymbolAts ats (sym :+ mempty) ]

extendSymbolAts        :: forall backend geom r.
                        (AttrOf backend geom ~ SymbolAttributes r)
                       => [Attr backend geom]
                       -> Ipe.IpeSymbol r :+ Ipe.IpeAttributes Ipe.IpeSymbol r
                       -> Ipe.IpeSymbol r :+ Ipe.IpeAttributes Ipe.IpeSymbol r
extendSymbolAts ats o' = o'&Ipe.attributes %~ \z -> foldrWithKey extend z (mconcat ats)
    where
      extend k (Identity val) acc = case k of
        SymbolCommon k' -> extendCommon @Ipe.IpeSymbol k' (Identity val) acc
        SymbolStroke    -> acc <> Ipe.attr Ipe.SStroke        val
        SymbolFill      -> acc <> Ipe.attr Ipe.SFill          val
        SymbolSize      -> acc <> Ipe.attr Ipe.SSize          val
        SymbolPen       -> acc <> Ipe.attr Ipe.SPen           val

instance ( GCompare (GroupAttributes r)
         ) => IsDrawable (Ipe r) (Ipe.Group r) where
  type AttrOf (Ipe r) (Ipe.Group r) = GroupAttributes r
  draw ats g = [ Ipe.IpeGroup $ g :+ ats' ]
    where
      ats' :: Ipe.IpeAttributes Ipe.Group r
      ats' = foldrWithKey extend' mempty (mconcat ats)
        where
          extend'                      :: GroupAttributes r v
                                       -> Identity v
                                       -> Ipe.IpeAttributes Ipe.Group r
                                       -> Ipe.IpeAttributes Ipe.Group r
          extend' k (Identity val) acc = case k of
            GroupCommon k' -> extendCommon @Ipe.Group k' (Identity val) acc
            Clip           -> acc <> Ipe.attr Ipe.SClip val

--------------------------------------------------------------------------------
-- * Defining the attributes

----------------------------------------
-- ** Common Attributes

class LayerAttr backend geom val | backend geom -> val where
  layer :: val -> Attr backend geom
class MatrixAttr backend geom val | backend geom -> val where
  matrix :: val -> Attr backend geom
class PinAttr backend geom val | backend geom -> val where
  pin :: val -> Attr backend geom
class TransformationsAttr backend geom val | backend geom -> val where
  transformations :: val -> Attr backend geom


----------------------------------------
-- * Path attributes

class StrokeAttr backend geom val | backend geom -> val where
  stroke :: val -> Attr backend geom
class FillAttr backend geom val | backend geom -> val where
  fill :: val -> Attr backend geom
class DashAttr backend geom val | backend geom -> val where
  dash :: val -> Attr backend geom
class PenAttr backend geom val | backend geom -> val where
  pen :: val -> Attr backend geom
class LineCapAttr backend geom val | backend geom -> val where
  lineCap :: val -> Attr backend geom
class LineJoinAttr backend geom val | backend geom -> val where
  lineJoin :: val -> Attr backend geom
class FillRuleAttr backend geom val | backend geom -> val where
  fillRule :: val -> Attr backend geom
class ArrowAttr backend geom val | backend geom -> val where
  arrow :: val -> Attr backend geom
class RArrowAttr backend geom val | backend geom -> val where
  rArrow :: val -> Attr backend geom
class StrokeOpacityAttr backend geom val | backend geom -> val where
  strokeOpacity :: val -> Attr backend geom
class OpacityAttr backend geom val | backend geom -> val where
  opacity :: val -> Attr backend geom
class GradientAttr backend geom val | backend geom -> val where
  gradient :: val -> Attr backend geom

----------------------------------------
-- * Text Attributes

class SizeAttr backend geom val | backend geom -> val where
  size :: val -> Attr backend geom
class WidthAttr  backend geom val | backend geom -> val where
  width :: val -> Attr backend geom
class HeightAttr backend geom val | backend geom -> val where
  height :: val -> Attr backend geom
class DepthAttr backend geom val | backend geom -> val where
  depth :: val -> Attr backend geom
class VAlignAttr backend geom val | backend geom -> val where
  vAlign :: val -> Attr backend geom
class HAlignAttr backend geom val | backend geom -> val where
  hAlign :: val -> Attr backend geom
class StyleAttr backend geom val | backend geom -> val where
  style :: val -> Attr backend geom

----------------------------------------
-- * Symbol Attributes
-- we already have classes for all Symbol attributes

----------------------------------------
-- * Group Attributes

class ClipAttr backend geom val | backend geom -> val where
  clip :: val -> Attr backend geom



class StrokeAndFillAttr backend geom val | backend geom -> val where
  -- | Sets both stroke and fill
  strokeAndFill :: val -> Attr backend geom


--------------------------------------------------------------------------------
-- * Instances

---------------------------------------- Path

instance LayerAttr (Ipe r) (Ipe.Path r) Ipe.LayerName where
  layer = singleton (PathCommon Layer)
instance MatrixAttr (Ipe r) (Ipe.Path r) (Matrix 3 3 r) where
  matrix = singleton (PathCommon Matrix)
instance PinAttr (Ipe r) (Ipe.Path r) Ipe.PinType where
  pin = singleton (PathCommon Pin)
instance TransformationsAttr (Ipe r) (Ipe.Path r) Ipe.TransformationTypes where
  transformations = singleton (PathCommon Transformations)
instance StrokeAttr (Ipe r) (Ipe.Path r) (Ipe.IpeColor r) where
  stroke = singleton PathStroke
instance FillAttr (Ipe r) (Ipe.Path r) (Ipe.IpeColor r) where
  fill  = singleton PathFill
instance DashAttr (Ipe r) (Ipe.Path r) (Ipe.IpeDash r) where
  dash = singleton Dash
instance PenAttr (Ipe r) (Ipe.Path r) (Ipe.IpePen r) where
  pen = singleton PathPen
instance LineCapAttr (Ipe r) (Ipe.Path r) Int where
  lineCap = singleton LineCap
instance LineJoinAttr (Ipe r) (Ipe.Path r) Int where
  lineJoin = singleton LineJoin
instance FillRuleAttr (Ipe r) (Ipe.Path r) Ipe.FillType where
  fillRule = singleton FillRule
instance ArrowAttr (Ipe r) (Ipe.Path r) (Ipe.IpeArrow r) where
  arrow = singleton Arrow
instance RArrowAttr (Ipe r) (Ipe.Path r) (Ipe.IpeArrow r) where
  rArrow = singleton RArrow
instance StrokeOpacityAttr (Ipe r) (Ipe.Path r) Ipe.IpeOpacity where
  strokeOpacity = singleton StrokeOpacity
instance OpacityAttr (Ipe r) (Ipe.Path r) Ipe.IpeOpacity where
  opacity = singleton Opacity
instance GradientAttr (Ipe r) (Ipe.Path r) Ipe.IpeGradient where
  gradient = singleton Gradient


----------------------------------------
instance LayerAttr (Ipe r) (Ipe.IpeSymbol r) Ipe.LayerName where
  layer = singleton (SymbolCommon Layer)
instance MatrixAttr (Ipe r) (Ipe.IpeSymbol r) (Matrix 3 3 r) where
  matrix = singleton (SymbolCommon Matrix)
instance PinAttr (Ipe r) (Ipe.IpeSymbol r) Ipe.PinType where
  pin = singleton (SymbolCommon Pin)
instance TransformationsAttr (Ipe r) (Ipe.IpeSymbol r) Ipe.TransformationTypes where
  transformations = singleton (SymbolCommon Transformations)
instance StrokeAttr (Ipe r) (Ipe.IpeSymbol r) (Ipe.IpeColor r) where
  stroke = singleton SymbolStroke
instance FillAttr (Ipe r) (Ipe.IpeSymbol r) (Ipe.IpeColor r) where
  fill  = singleton SymbolFill
instance PenAttr (Ipe r) (Ipe.IpeSymbol r) (Ipe.IpePen r) where
  pen = singleton SymbolPen
instance SizeAttr (Ipe r) (Ipe.IpeSymbol r) (Ipe.IpeSize r)  where
  size = singleton SymbolSize









instance LayerAttr (Ipe r) (Ipe.IpeObject r) Ipe.LayerName where
  layer = singleton Layer




    -- mapKeys PathCommon . layer

-- instance HasLayer geom (Ipe r) Ipe.LayerName where
--   layer = singleton

--------------------------------------------------------------------------------

instance ( Point_ point 2 r
         , SimplePolygon_ (SimplePolygonF f point) point r
         ) => IsDrawable (Ipe r) (SimplePolygonF f point) where
  type AttrOf (Ipe r) (SimplePolygonF f point) = PathAttributes r
  draw ats poly = [ Ipe.iO $ extendPathAts ats (Ipe.ipeSimplePolygon poly)
                  ]

instance IsDrawable (Ipe r) (Point 2 r) where
  type AttrOf (Ipe r) (Point 2 r) = SymbolAttributes r
  draw ats p = [ Ipe.iO $ extendSymbolAts ats (Ipe.ipeDiskMark p)
               ]


    -- let path :+ ats' =
    --               in [ IpePath path :+ foldrWithKey extendPathAts ats' (mconcat ats)
    --                  ]

-- test :: [IpeObject Double]
-- test = draw [ layer "intersection"
--             ]
--             myPath

-- myPath = Ipe.Path
