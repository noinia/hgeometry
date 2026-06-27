{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
module Ipe.Content(
    Image(Image), imageData, imageRect
  , TextLabel(..)
  , MiniPage(..), width

  , IpeSymbol(Symbol), symbolPoint, symbolName

  , Path(Path), pathSegments
  , singletonPath
  , PathSegment(..)

  , Group(Group), groupItems


  , IpeObject(..), _IpeGroup, _IpeImage, _IpeTextLabel, _IpeMiniPage, _IpeUse, _IpePath
  , IpeObject'
  , ipeObject', ToObject(..)

  , IpeAttributes
  , attributes

  , flattenGroups
  ) where

import           Ipe.Value
import           Data.Bitraversable
import           Control.Lens hiding (views, elements)
import           Data.Kind
import           Data.Proxy
import           Data.Text (Text)
import           Data.Traversable
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
import           Ipe.Attributes.Types

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


--------------------------------------------------------------------------------
-- * Paths are in a separate module


--------------------------------------------------------------------------------



--------------------------------------------------------------------------------
-- | Groups and Objects

--------------------------------------------------------------------------------
-- | Group Attributes



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

type family IpeAttributes (g :: Type -> Type) (r :: Type) :: Type where
  IpeAttributes Group     r = GroupAttributes r
  IpeAttributes Image     r = ImageAttributes r
  IpeAttributes TextLabel r = TextAttributes r
  IpeAttributes MiniPage  r = TextAttributes r
  IpeAttributes Path      r = PathAttributes r
  IpeAttributes IpeSymbol r = SymbolAttributes r


-- | An IpeObject' is essentially the oject ogether with its attributes
type IpeObject' g r = g r :+ IpeAttributes g r


data IpeObject r =
    IpeGroup     (IpeObject' Group     r)
  | IpeImage     (IpeObject' Image     r)
  | IpeTextLabel (IpeObject' TextLabel r)
  | IpeMiniPage  (IpeObject' MiniPage  r)
  | IpeUse       (IpeObject' IpeSymbol r)
  | IpePath      (IpeObject' Path      r)
  deriving (Generic)

instance Functor IpeObject where
  fmap = fmapDefault
instance Foldable IpeObject where
  foldMap = foldMapDefault
instance Traversable IpeObject where
  traverse f = \case
    IpeGroup g     -> IpeGroup     <$> bitraverse (traverse f) (traverseGroup f) g
    IpeImage i     -> IpeImage     <$> bitraverse (traverse f) (traverseCommon f) i
    IpeTextLabel l -> IpeTextLabel <$> bitraverse (traverse f) (traverseText f) l
    IpeMiniPage p  -> IpeMiniPage  <$> bitraverse (traverse f) (traverseText f) p
    IpeUse u       -> IpeUse       <$> bitraverse (traverse f) (traverseSymbol f) u
    IpePath p      -> IpePath      <$> bitraverse (traverse f) (traversePath f) p


deriving instance (Show r) => Show (IpeObject r)
-- deriving instance (Read r) => Read (IpeObject r)
deriving instance (Eq r)   => Eq   (IpeObject r)

type instance NumType   (IpeObject r) = r
type instance Dimension (IpeObject r) = 2

makePrisms ''IpeObject

groupItems :: Lens (Group r) (Group s) [IpeObject r] [IpeObject s]
groupItems = lens (\(Group xs) -> xs) (const Group)

class ToObject i where
  _IpeObject  :: Prism' (IpeObject r) (i r :+ IpeAttributes i r)

  mkIpeObject :: IpeObject' i r -> IpeObject r
  mkIpeObject = review _IpeObject

instance ToObject Group      where _IpeObject = _IpeGroup
instance ToObject Image      where _IpeObject = _IpeImage
instance ToObject TextLabel  where _IpeObject = _IpeTextLabel
instance ToObject MiniPage   where _IpeObject = _IpeMiniPage
instance ToObject IpeSymbol  where _IpeObject = _IpeUse
instance ToObject Path       where _IpeObject = _IpePath


-- | Shorthand for constructing ipeObjects
ipeObject'     :: ToObject i => i r -> IpeAttributes i r -> IpeObject r
ipeObject' i a = review _IpeObject $ i :+ a


-- Applicative f => (ipeAttributes i r -> f (IpeAttributes i r :+ ats))
--            -> IpeObject r -> f (IpeObject r)
 -- s s a a

-- | Access the attributes
attributes :: Lens' (IpeObject' i r) (IpeAttributes i r)
attributes = extra

-- | Access the attributes
instance HasCommonAttributes (IpeObject r) r Maybe where
-- commonAttributes'      :: Lens' (IpeObject r) (CommonAttributes r Maybe)
  commonAttributes fAts = \case
    IpeGroup g     -> IpeGroup     <$> (extra.commonAttributes) fAts g
    IpeImage i     -> IpeImage     <$> (extra.commonAttributes) fAts i
    IpeTextLabel l -> IpeTextLabel <$> (extra.commonAttributes) fAts l
    IpeMiniPage p  -> IpeMiniPage  <$> (extra.commonAttributes) fAts p
    IpeUse u       -> IpeUse       <$> (extra.commonAttributes) fAts u
    IpePath p      -> IpePath      <$> (extra.commonAttributes) fAts p




  -- _IpeObject @i.extra


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
