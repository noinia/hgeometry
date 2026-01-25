{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
module Ipe.Content(
    Image(Image), imageData, imageRect
  , TextLabel(..)
  , MiniPage(..), width

  , IpeSymbol(Symbol), symbolPoint, symbolName

  , Path(Path), pathSegments
  , PathSegment(..)

  , Group(Group), groupItems


  , IpeObject(..), _IpeGroup, _IpeImage, _IpeTextLabel, _IpeMiniPage, _IpeUse, _IpePath
  , IpeObject'
  , ipeObject', ToObject(..)

  , IpeAttributes
  , Attributes', AttributesOf, AttrMap, AttrMapSym1
  , attributes, mapIpeAttrs, traverseIpeAttrs
  , commonAttributes

  , flattenGroups
  ) where

import           Control.Lens hiding (views, elements)
import           Data.Kind
import           Data.Proxy
import           Data.Singletons.TH (genDefunSymbols)
import           Data.Text (Text)
import           Data.Traversable
import           Data.Vinyl hiding (Label)
import           Data.Vinyl.TypeLevel (AllConstrained)
import           GHC.Generics (Generic)
import           HGeometry.Box (Rectangle)
import           HGeometry.Ext
import           HGeometry.Matrix
import           HGeometry.Point
import           HGeometry.Properties
import           HGeometry.Transformation
import qualified Ipe.Attributes as AT
import           Ipe.Attributes hiding (Matrix)
import           Ipe.Color
import           Ipe.Layer
import           Ipe.Path


--------------------------------------------------------------------------------
-- | Image Objects

-- | bitmap image objects in Ipe
data Image r = Image { _imageData :: ()
                     , _imageRect      :: Rectangle (Point 2 r)
                     } deriving (Show,Eq,Ord,Generic)

-- | Lens to access the image data
imageData :: Lens' (Image r) ()
imageData f (Image i r) = fmap (\i' -> Image i' r) (f i)
{-# INLINE imageData #-}

-- | Lens to access the rectangle of the image
imageRect :: Lens (Image r) (Image r') (Rectangle (Point 2 r)) (Rectangle (Point 2 r'))
imageRect f (Image i r) = fmap (\r' -> Image i r') (f r)
{-# INLINE imageRect #-}

type instance NumType   (Image r) = r
type instance Dimension (Image r) = 2

instance Fractional r => IsTransformable (Image r) where
  transformBy t = over imageRect (transformBy t)

instance Functor Image where
  fmap = fmapDefault
instance Foldable Image where
  foldMap = foldMapDefault
instance Traversable Image where
  traverse f (Image d r) = Image d <$> traverse (cloneTraversal coordinates f) r

--------------------------------------------------------------------------------
-- | Text Objects

-- | A text label
data TextLabel r = Label Text (Point 2 r)
                 deriving (Show,Eq,Ord,Generic)

type instance NumType   (TextLabel r) = r
type instance Dimension (TextLabel r) = 2

instance Functor TextLabel  where fmap = fmapDefault
instance Foldable TextLabel where foldMap = foldMapDefault
instance Traversable TextLabel where
  traverse f (Label t p) = let coordinates' = cloneTraversal coordinates
                           in Label t <$> coordinates' f p

instance Fractional r => IsTransformable (TextLabel r) where
  transformBy t (Label txt p) = Label txt (transformBy t p)


-- | A Minipage
data MiniPage r = MiniPage Text (Point 2 r) r
                 deriving (Show,Eq,Ord,Generic)

type instance NumType   (MiniPage r) = r
type instance Dimension (MiniPage r) = 2

instance Functor MiniPage  where fmap = fmapDefault
instance Foldable MiniPage where foldMap = foldMapDefault
instance Traversable MiniPage where
  traverse f (MiniPage t p w) = let coordinates' = cloneTraversal coordinates
                                in MiniPage t <$> coordinates' f p <*> f w

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
                 deriving (Show,Eq,Ord,Generic)

-- | Lens to access the position of the symbol
symbolPoint :: Lens (IpeSymbol r) (IpeSymbol r') (Point 2 r) (Point 2 r')
symbolPoint f (Symbol p n) = fmap (\p' -> Symbol p' n) (f p)
{-# INLINE symbolPoint #-}

-- | Lens to access the name of the symbol
symbolName :: Lens' (IpeSymbol r) Text
symbolName f (Symbol p n) = fmap (\n' -> Symbol p n') (f n)
{-# INLINE symbolName #-}


type instance NumType   (IpeSymbol r) = r
type instance Dimension (IpeSymbol r) = 2

instance Functor IpeSymbol  where fmap = fmapDefault
instance Foldable IpeSymbol where foldMap = foldMapDefault
instance Traversable IpeSymbol where
  traverse f (Symbol p t) = let coordinates' = cloneTraversal coordinates
                            in flip Symbol t <$> coordinates' f p

instance Fractional r => IsTransformable (IpeSymbol r) where
  transformBy t = over symbolPoint (transformBy t)



-- | Example of an IpeSymbol. I.e. A symbol that expresses that the size is 'large'
-- sizeSymbol :: Attributes (AttrMapSym1 r) (SymbolAttributes r)
-- sizeSymbol = attr SSize (IpeSize $ Named "large")


--------------------------------------------------------------------------------
-- * Paths are in a separate module

--------------------------------------------------------------------------------
-- * Attribute Mapping


-- | The mapping between the labels of the the attributes and the types of the
-- attributes with these labels. For example, the 'Matrix' label/attribute should
-- have a value of type 'Matrix 3 3 r'.
type family AttrMap (r :: Type) (l :: AttributeUniverse) :: Type where
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
  AttrMap r StrokeOpacity = IpeOpacity
  AttrMap r Opacity       = IpeOpacity
  AttrMap r Tiling        = IpeTiling
  AttrMap r Gradient      = IpeGradient

  AttrMap r Width          = TextSizeUnit r
  AttrMap r Height         = TextSizeUnit r
  AttrMap r Depth          = TextSizeUnit r
  AttrMap r VAlign         = VerticalAlignment
  AttrMap r HAlign         = HorizontalAlignment
  AttrMap r Style          = TeXStyle

  AttrMap r Clip = Path r -- strictly we event want this to be a closed path I guess



genDefunSymbols [''AttrMap]

--------------------------------------------------------------------------------


-- | For the types representing attribute values we can get the name/key to use
-- when serializing to ipe.
class TraverseIpeAttr (a :: AttributeUniverse) where
  traverseIpeAttr :: Applicative h
                  => (r -> h s) -> Attr (AttrMapSym1 r) a -> h (Attr (AttrMapSym1 s) a)

  -- attrName :: proxy a -> Text

-- CommonAttributeUnivers
instance TraverseIpeAttr Layer           where traverseIpeAttr _ = pureAttr
instance TraverseIpeAttr AT.Matrix       where traverseIpeAttr f = traverseAttr (cloneTraversal elements f)
instance TraverseIpeAttr Pin             where traverseIpeAttr _ = pureAttr
instance TraverseIpeAttr Transformations where traverseIpeAttr _ = pureAttr

-- -- IpeSymbolAttributeUniversre
instance TraverseIpeAttr Stroke       where traverseIpeAttr f = traverseAttr (traverse f)
instance TraverseIpeAttr Fill         where traverseIpeAttr f = traverseAttr (traverse f)
instance TraverseIpeAttr Pen          where traverseIpeAttr f = traverseAttr (traverse f)
instance TraverseIpeAttr Size         where traverseIpeAttr f = traverseAttr (traverse f)

-- -- PathAttributeUniverse
instance TraverseIpeAttr Dash       where traverseIpeAttr f = traverseAttr (traverse f)
instance TraverseIpeAttr LineCap    where traverseIpeAttr _ = pureAttr
instance TraverseIpeAttr LineJoin   where traverseIpeAttr _ = pureAttr
instance TraverseIpeAttr FillRule   where traverseIpeAttr _ = pureAttr
instance TraverseIpeAttr Arrow      where traverseIpeAttr f = traverseAttr (traverse f)
instance TraverseIpeAttr RArrow     where traverseIpeAttr f = traverseAttr (traverse f)
instance TraverseIpeAttr StrokeOpacity  where traverseIpeAttr _ = pureAttr
instance TraverseIpeAttr Opacity    where traverseIpeAttr _ = pureAttr
instance TraverseIpeAttr Tiling     where traverseIpeAttr _ = pureAttr
instance TraverseIpeAttr Gradient   where traverseIpeAttr _ = pureAttr

-- TextAttibuteUniverse
instance TraverseIpeAttr Width   where traverseIpeAttr f = traverseAttr (traverse f)
instance TraverseIpeAttr Height  where traverseIpeAttr f = traverseAttr (traverse f)
instance TraverseIpeAttr Depth   where traverseIpeAttr f = traverseAttr (traverse f)
instance TraverseIpeAttr VAlign  where traverseIpeAttr _ = pureAttr
instance TraverseIpeAttr HAlign  where traverseIpeAttr _ = pureAttr
instance TraverseIpeAttr Style   where traverseIpeAttr _ = pureAttr


-- GroupAttributeUniverse
instance TraverseIpeAttr Clip     where traverseIpeAttr f = traverseAttr (traverse f)

--------------------------------------------------------------------------------
-- | Groups and Objects

--------------------------------------------------------------------------------
-- | Group Attributes

-- -- | Now that we know what a Path is we can define the Attributes of a Group.
-- type family GroupAttrElf (r :: Type) (s :: GroupAttributeUniverse) :: Type where
--   GroupAttrElf r Clip = Path r -- strictly we event want this to be a closed path I guess

-- genDefunSymbols [''GroupAttrElf]

-- type GroupAttributes r = Attributes (GroupAttrElfSym1 r) '[ 'Clip]


-- | A group is essentially a list of IpeObjects.
newtype Group r = Group [IpeObject r]
  deriving (Show,Eq,Functor,Foldable,Traversable,Generic)

type instance NumType   (Group r) = r
type instance Dimension (Group r) = 2

instance (Fractional r, Eq r) => IsTransformable (IpeObject r) where
  transformBy t (IpeGroup i)     = IpeGroup     $ i&core %~ transformBy t
  transformBy t (IpeImage i)     = IpeImage     $ i&core %~ transformBy t
  transformBy t (IpeTextLabel i) = IpeTextLabel $ i&core %~ transformBy t
  transformBy t (IpeMiniPage i)  = IpeMiniPage  $ i&core %~ transformBy t
  transformBy t (IpeUse i)       = IpeUse       $ i&core %~ transformBy t
  transformBy t (IpePath i)      = IpePath      $ i&core %~ transformBy t

instance (Fractional r, Eq r) => IsTransformable (Group r) where
  transformBy t (Group s) = Group $ fmap (transformBy t) s



type family AttributesOf (t :: Type -> Type) :: [AttributeUniverse] where
  AttributesOf Group     = GroupAttributes
  AttributesOf Image     = ImageAttributes
  AttributesOf TextLabel = TextLabelAttributes
  AttributesOf MiniPage  = MiniPageAttributes
  AttributesOf IpeSymbol = SymbolAttributes
  AttributesOf Path      = PathAttributes


-- | Attributes' :: Type -> [AttributeUniverse] -> Type
type Attributes' r = Attributes (AttrMapSym1 r)

type IpeAttributes g r = Attributes' r (AttributesOf g)


-- | An IpeObject' is essentially the oject ogether with its attributes
type IpeObject' g r = g r :+ IpeAttributes g r

attributes :: Lens' (IpeObject' g r) (IpeAttributes g r)
attributes = extra

-- | Map some function over the coordinates of the ipe Attributes
mapIpeAttrs      :: AllConstrained TraverseIpeAttr (AttributesOf g)
                 => proxy g -> (r -> s) -> IpeAttributes g r -> IpeAttributes g s
mapIpeAttrs px f = runIdentity . traverseIpeAttrs px (Identity . f)

-- | Traverse for ipe attributes
traverseIpeAttrs               :: ( Applicative f
                                  , AllConstrained TraverseIpeAttr (AttributesOf g)
                                  ) => proxy g -> (r -> f s) -> IpeAttributes g r -> f (IpeAttributes g s)
traverseIpeAttrs _ f (Attrs ats) = fmap Attrs . traverseIpeAttrs' f $ ats

traverseIpeAttrs'   :: ( Applicative f
                       , AllConstrained TraverseIpeAttr ats
                       )
                    => (r -> f s)
                    -> Rec (Attr (AttrMapSym1 r)) ats
                    -> f (Rec (Attr (AttrMapSym1 s)) ats)
traverseIpeAttrs' f = \case
  RNil        -> pure RNil
  (a :& ats') -> (:&) <$> traverseIpeAttr f a <*> traverseIpeAttrs' f ats'


data IpeObject r =
    IpeGroup     (IpeObject' Group     r)
  | IpeImage     (IpeObject' Image     r)
  | IpeTextLabel (IpeObject' TextLabel r)
  | IpeMiniPage  (IpeObject' MiniPage  r)
  | IpeUse       (IpeObject' IpeSymbol r)
  | IpePath      (IpeObject' Path      r)
  deriving (Generic)

traverseIpeObject'              :: forall g r f s. ( Applicative f
                                                   , Traversable g
                                                   , AllConstrained TraverseIpeAttr (AttributesOf  g)
                                                   )
                                => (r -> f s) -> IpeObject' g r -> f (IpeObject' g s)
traverseIpeObject' f (i :+ ats) = (:+) <$> traverse f i <*> traverseIpeAttrs (Proxy @g) f ats

instance Functor IpeObject where
  fmap = fmapDefault
instance Foldable IpeObject where
  foldMap = foldMapDefault
instance Traversable IpeObject where
  traverse f = \case
    IpeGroup g     -> IpeGroup     <$> traverseIpeObject' f g
    IpeImage i     -> IpeImage     <$> traverseIpeObject' f i
    IpeTextLabel l -> IpeTextLabel <$> traverseIpeObject' f l
    IpeMiniPage p  -> IpeMiniPage  <$> traverseIpeObject' f p
    IpeUse u       -> IpeUse       <$> traverseIpeObject' f u
    IpePath p      -> IpePath      <$> traverseIpeObject' f p


deriving instance (Show r) => Show (IpeObject r)
-- deriving instance (Read r) => Read (IpeObject r)
deriving instance (Eq r)   => Eq   (IpeObject r)

type instance NumType   (IpeObject r) = r
type instance Dimension (IpeObject r) = 2

makePrisms ''IpeObject

groupItems :: Lens (Group r) (Group s) [IpeObject r] [IpeObject s]
groupItems = lens (\(Group xs) -> xs) (const Group)

class ToObject i where
  mkIpeObject :: IpeObject' i r -> IpeObject r

instance ToObject Group      where mkIpeObject = IpeGroup
instance ToObject Image      where mkIpeObject = IpeImage
instance ToObject TextLabel  where mkIpeObject = IpeTextLabel
instance ToObject MiniPage   where mkIpeObject = IpeMiniPage
instance ToObject IpeSymbol  where mkIpeObject = IpeUse
instance ToObject Path       where mkIpeObject = IpePath


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
