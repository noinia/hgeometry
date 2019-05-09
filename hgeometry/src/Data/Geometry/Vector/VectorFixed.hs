{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Geometry.Vector.VectorFixed where

import           Control.DeepSeq
import           Control.Lens hiding (element)
import           Data.Aeson
import qualified Data.Foldable as F
import           Data.Proxy
import qualified Data.Vector.Fixed as V
import           Data.Vector.Fixed (Arity)
import           Data.Vector.Fixed.Boxed
import           GHC.Generics (Generic)
import           GHC.TypeLits
import           Linear.Affine (Affine(..))
import           Linear.Metric
import qualified Linear.V2 as L2
import qualified Linear.V3 as L3
import           Linear.Vector

--------------------------------------------------------------------------------

-- | A proxy which can be used for the coordinates.
data C (n :: Nat) = C deriving (Show,Read,Eq,Ord)

--------------------------------------------------------------------------------
-- * d dimensional Vectors

-- | Datatype representing d dimensional vectors. Our implementation wraps the
-- implementation provided by fixed-vector.
newtype Vector (d :: Nat)  (r :: *) = Vector { _unV :: Vec d r }
                                    deriving (Generic)

unV :: Lens' (Vector d r) (Vec d r)
unV = lens _unV (const Vector)

----------------------------------------

-- | Lens into the i th element
element   :: forall proxy i d r. (Arity d, Arity i, (i + 1) <= d)
          => proxy i -> Lens' (Vector d r) r
element _ = V.elementTy (Proxy :: Proxy i)

-- | Similar to 'element' above. Except that we don't have a static guarantee
-- that the index is in bounds. Hence, we can only return a Traversal
element'   :: forall d r. Arity d => Int -> Traversal' (Vector d r) r
element' i f v
  | 0 <= i && i < fromInteger (natVal (C :: C d)) = f (v V.! i)
                                                 <&> \a -> (v&V.element i .~ a)
       -- Implementation based on that of Ixed Vector in Control.Lens.At
  | otherwise                                     = pure v


vectorFromList :: Arity d => [a] -> Maybe (Vector d a)
vectorFromList = fmap Vector . V.fromListM

vectorFromListUnsafe :: Arity d => [a] -> Vector d a
vectorFromListUnsafe = Vector . V.fromList


instance (Show r, Arity d) => Show (Vector d r) where
  show (Vector v) = mconcat [ "Vector", show $ V.length v , " "
                            , show $ F.toList v
                            ]

deriving instance (Eq r, Arity d)   => Eq (Vector d r)
deriving instance (Ord r, Arity d)  => Ord (Vector d r)
-- deriving instance Arity d  => Functor (Vector d)

-- for some weird reason, implemeting this myself yields is faster code
instance Arity d  => Functor (Vector d) where
  fmap f (Vector v) = Vector $ fmap f v

deriving instance Arity d  => Foldable (Vector d)
deriving instance Arity d  => Applicative (Vector d)

instance Arity d => Traversable (Vector d) where
  traverse f (Vector v) = Vector <$> traverse f v

deriving instance (Arity d, NFData r) => NFData (Vector d r)


instance Arity d => Additive (Vector d) where
  zero = pure 0
  (Vector u) ^+^ (Vector v) = Vector $ V.zipWith (+) u v

instance Arity d => Affine (Vector d) where
  type Diff (Vector d) = Vector d

  u .-. v = u ^-^ v
  p .+^ v = p ^+^ v


instance Arity d => Metric (Vector d)

type instance V.Dim (Vector d) = d

instance Arity d => V.Vector (Vector d) r where
  construct  = Vector <$> V.construct
  inspect    = V.inspect . _unV
  basicIndex = V.basicIndex . _unV

instance (FromJSON r, Arity d, KnownNat d)  => FromJSON (Vector d r) where
  parseJSON y = parseJSON y >>= \xs -> case vectorFromList xs of
                  Nothing -> fail . mconcat $
                    [ "FromJSON (Vector d a), wrong number of elements. Expected "
                    , show $ natVal (Proxy :: Proxy d)
                    , " elements but found "
                    , show $ length xs
                    , "."
                    ]
                  Just v -> pure v

instance (ToJSON r, Arity d) => ToJSON (Vector d r) where
  toJSON     = toJSON     . F.toList
  toEncoding = toEncoding . F.toList

------------------------------------------

-- | Get the head and tail of a vector
destruct            :: (Arity d, Arity (d + 1), 1 <= (d + 1))
                    => Vector (d + 1) r -> (r, Vector d r)
destruct (Vector v) = (V.head v, Vector $ V.tail v)


-- | Cross product of two three-dimensional vectors
cross       :: Num r => Vector 3 r -> Vector 3 r -> Vector 3 r
u `cross` v = fromV3 $ (toV3 u) `L3.cross` (toV3 v)


--------------------------------------------------------------------------------

-- | Vonversion to a Linear.V2
toV2                :: Vector 2 a -> L2.V2 a
toV2 ~(Vector2 a b) = L2.V2 a b

-- | Conversion to a Linear.V3
toV3                  :: Vector 3 a -> L3.V3 a
toV3 ~(Vector3 a b c) = L3.V3 a b c

-- | Conversion from a Linear.V3
fromV3               :: L3.V3 a -> Vector 3 a
fromV3 (L3.V3 a b c) = v3 a b c

----------------------------------------------------------------------------------

-- | Add an element at the back of the vector
snoc :: (Arity (d + 1), Arity d) => Vector d r -> r -> Vector (d + 1) r
snoc = flip V.snoc

-- | Get a vector of the first d - 1 elements.
init :: (Arity d, Arity (d + 1)) => Vector (d + 1) r -> Vector d r
init = Vector . V.reverse . V.tail . V.reverse . _unV

last :: forall d r. (Arity d, Arity (d + 1)) => Vector (d + 1) r -> r
last = view $ element (Proxy :: Proxy d)

-- | Get a prefix of i elements of a vector
prefix :: forall i d r. (Arity d, Arity i, i <= d)
       => Vector d r -> Vector i r
prefix = let i = fromInteger . natVal $ (Proxy :: Proxy i)
         in V.fromList . take i . V.toList

--------------------------------------------------------------------------------
-- * Functions specific to two and three dimensional vectors.

-- | Construct a 2 dimensional vector
v2     :: r -> r -> Vector 2 r
v2 a b = Vector $ V.mk2 a b

-- | Construct a 3 dimensional vector
v3      :: r -> r -> r -> Vector 3 r
v3 a b c = Vector $ V.mk3 a b c

-- | Destruct a 2 dim vector into a pair
_unV2 :: Vector 2 r -> (r,r)
_unV2 v = let [x,y] = V.toList v in (x,y)

_unV3 :: Vector 3 r -> (r,r,r)
_unV3 v = let [x,y,z] = V.toList v in (x,y,z)

-- | Pattern synonym for two and three dim vectors
pattern Vector2       :: r -> r -> Vector 2 r
pattern Vector2 x y   <- (_unV2 -> (x,y))
  where
    Vector2 x y = v2 x y
{-# COMPLETE Vector2 #-}

pattern Vector3       :: r -> r -> r -> Vector 3 r
pattern Vector3 x y z <- (_unV3 -> (x,y,z))
  where
    Vector3 x y z = v3 x y z
{-# COMPLETE Vector3 #-}


pattern Vector4         :: r -> r -> r -> r -> Vector 4 r
pattern Vector4 x y z a <- (V.toList -> [x,y,z,a])
  where
    Vector4 x y z a = V.mk4 x y z a
{-# COMPLETE Vector4 #-}
