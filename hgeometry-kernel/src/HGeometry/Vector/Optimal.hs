{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
module HGeometry.Vector.Optimal
  ( Vector, pattern Vector1, pattern Vector2, pattern Vector3, pattern Vector4
  , VectorImpl
  , Choose
  , Optimize(..)
  , ChooseBoxed
  , Boxing(..)
  ) where

import           Control.Applicative
import           Control.DeepSeq
import           Control.Lens
import           Data.Coerce
import           Data.Functor.Classes
import           Data.Kind
import           Data.Proxy
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import           GHC.Generics
import           GHC.TypeLits
import           HGeometry.Properties
import           HGeometry.Vector.Class
import           HGeometry.Vector.Helper
import           HGeometry.Vector.Optimal.Defs
import qualified Linear.V2 as L2
import qualified Linear.V3 as L3
import qualified Linear.V4 as L4
import           Text.Read (Read(..),readListPrecDefault)
--------------------------------------------------------------------------------

-- | d-dimensional vectors that pick a specialized implementation for sizes up to 4.
type Vector d r = VectorImpl (Choose d) (ChooseBoxed r) r

--------------------------------------------------------------------------------
-- * Convenience "constructors"

-- | Constant sized vector with d elements.
-- pattern Vector   :: VectorFamilyF (Peano d) r -> Vector d r
-- pattern Vector v = MKVector (VectorFamily v)
-- {-# COMPLETE Vector #-}

-- | Constant sized vector with 1 element.
pattern Vector1   :: r -> Vector 1 r
pattern Vector1 x <- (coerce -> x)
  where
    Vector1 = coerce

{-# COMPLETE Vector1 #-}

-- | Constant sized vector with 2 elements.
pattern Vector2     :: r -> r -> VectorImpl Two Boxed r
pattern Vector2 x y = (V_2 (L2.V2 x y))
{-# COMPLETE Vector2 #-}

-- | Constant sized vector with 3 elements.
pattern Vector3        :: r -> r -> r -> VectorImpl Three Boxed r
pattern Vector3 x y z  = (V_3 (L3.V3 x y z))
{-# COMPLETE Vector3 #-}

-- | Constant sized vector with 4 elements.
pattern Vector4         :: r -> r -> r -> r -> VectorImpl Four Boxed r
pattern Vector4 x y z w = (V_4 (L4.V4 x y z w))
{-# COMPLETE Vector4 #-}

-- -- | Constant sized vector with 4 elements.
-- pattern Vector   :: V.Vector r -> Vector d r
-- pattern Vector v = (V_LargeB v)
-- {-# COMPLETE Vector #-}



--------------------------------------------------------------------------------

-- | Whether the vector is unboxed or boxed.
data Boxing = Unboxed | Boxed deriving (Show,Eq)

-- | Specify whether to use a boxed (default) or unboxed vector for a
-- given number type.
type ChooseBoxed :: Type -> Boxing
type family ChooseBoxed r where
  ChooseBoxed Int    = Unboxed
  ChooseBoxed Word   = Unboxed
  ChooseBoxed Float  = Unboxed
  ChooseBoxed Double = Unboxed
  ChooseBoxed r      = Boxed

-- | The implementation of the vector, we can pick an optimal
-- representation for a given dimension and type.
type VectorImpl :: ByDimension Nat -> Boxing -> Type -> Type
data family VectorImpl o b r

--------------------------------------------------------------------------------

type instance NumType (VectorImpl o b r) = r
type instance IxValue (VectorImpl o b r) = r
type instance Index   (VectorImpl o b r) = Int

type instance Dimension (VectorImpl Zero      b r) = 0
type instance Dimension (VectorImpl One       b r) = 1
type instance Dimension (VectorImpl Two       b r) = 2
type instance Dimension (VectorImpl Three     b r) = 3
type instance Dimension (VectorImpl Four      b r) = 4
type instance Dimension (VectorImpl (Large d) b r) = d

--------------------------------------------------------------------------------
-- * Implementations

-- | Zero dimensional vectors (boring)
newtype instance VectorImpl Zero b r'      = V_0 ()
  deriving (Eq,Ord,NFData,Generic,Functor,Foldable,Traversable)

-- | One dimensional vectors
newtype instance VectorImpl One b r     = V_1 (Identity r)
  deriving (Eq,Eq1,Ord,Ord1,NFData,Generic,Functor,Foldable,Traversable,Applicative)

--------------------------------------------------------------------------------
-- * Boxed implementations

-- | Two dimensional boxed vectors
newtype instance VectorImpl Two Boxed r       = V_2 (L2.V2 r)
  deriving (Eq,Eq1,Ord,Ord1,NFData,Generic,Functor,Foldable,Traversable,Applicative)

-- | Three dimensional boxed vectors
newtype instance VectorImpl Three Boxed r     = V_3 (L3.V3 r)
  deriving (Eq,Eq1,Ord,Ord1,NFData,Generic,Functor,Foldable,Traversable,Applicative)

-- | Four dimensional boxed vectors
newtype instance VectorImpl Four Boxed r      = V_4 (L4.V4 r)
  deriving (Eq,Eq1,Ord,Ord1,NFData,Generic,Functor,Foldable,Traversable,Applicative)

-- | Large (dimension alrger than four) boxed vectors
newtype instance VectorImpl (Large d) Boxed r = V_LargeB (V.Vector r)
  deriving (Eq,Eq1,Ord,Ord1,NFData,Generic,Functor,Foldable,Traversable,Applicative)

--------------------------------------------------------------------------------
-- * Unboxed implementations, specialized to each type


-- | Large unboxed vectors of an unboxable type
newtype instance VectorImpl (Large d) Unboxed r = V_LargeUB (UV.Vector r)
  deriving (Eq,Ord,NFData,Generic)

--------------------------------------------------------------------------------

instance Eq1 (VectorImpl Zero b) where
  liftEq _ _ _ = True

instance Ord1 (VectorImpl Zero b) where
  liftCompare _ _ _ = EQ

instance Applicative (VectorImpl Zero b) where
  pure _ = V_0 ()
  _ <*> _ = V_0 ()

instance Ixed (VectorImpl Zero b r    ) where
  ix _ _ = pure

instance Ixed (VectorImpl One b r) where
  ix i f v@(V_1 (Identity x)) = case i of
                       0 -> coerce <$> f x
                       _ -> pure v
  {-# INLINE ix #-}


instance Ixed (VectorImpl Two Boxed r      ) where
  ix i f v = ix i f (coerce v)
  {-# INLINE ix #-}

instance Ixed (VectorImpl Three Boxed r    ) where
  ix i f v = ix i f (coerce v)
  {-# INLINE ix #-}

instance Ixed (VectorImpl Four Boxed r     ) where
  ix i f v = ix i f (coerce v)
  {-# INLINE ix #-}

instance Ixed (VectorImpl (Large d) b r) where
  ix i f v = ix i f (coerce v)
  {-# INLINE ix #-}

instance FunctorWithIndex Int (VectorImpl Zero b)
instance FunctorWithIndex Int (VectorImpl One  b)
instance FunctorWithIndex Int (VectorImpl Two Boxed)
instance FunctorWithIndex Int (VectorImpl Three Boxed)
instance FunctorWithIndex Int (VectorImpl Four Boxed)
instance FunctorWithIndex Int (VectorImpl (Large d) Boxed)

instance FoldableWithIndex Int (VectorImpl Zero b)
instance FoldableWithIndex Int (VectorImpl One b)
instance FoldableWithIndex Int (VectorImpl Two Boxed)
instance FoldableWithIndex Int (VectorImpl Three Boxed)
instance FoldableWithIndex Int (VectorImpl Four Boxed)
instance FoldableWithIndex Int (VectorImpl (Large d) Boxed)

instance TraversableWithIndex Int (VectorImpl Zero b)
instance TraversableWithIndex Int (VectorImpl One b)
instance TraversableWithIndex Int (VectorImpl Two Boxed)
instance TraversableWithIndex Int (VectorImpl Three Boxed)
instance TraversableWithIndex Int (VectorImpl Four Boxed)
instance TraversableWithIndex Int (VectorImpl (Large d)  Boxed)

--------------------------------------------------------------------------------
-- * HasComponents instances

instance TraversableWithIndex Int (VectorImpl o Boxed)
         => HasComponents (VectorImpl o Boxed r) (VectorImpl o Boxed s) where
  {-# SPECIALIZE instance HasComponents (VectorImpl Two Boxed r)       (VectorImpl Two Boxed s) #-}
  {-# SPECIALIZE instance HasComponents (VectorImpl Three Boxed r)     (VectorImpl Three Boxed s) #-}
  {-# SPECIALIZE instance HasComponents (VectorImpl Four Boxed r)      (VectorImpl Four Boxed s) #-}
  {-# SPECIALIZE instance HasComponents (VectorImpl (Large d) Boxed r) (VectorImpl (Large d) Boxed s) #-}
  components = itraversed



instance HasComponents (VectorImpl Zero b r)
                       (VectorImpl Zero b r) where
  components = itraversed

instance HasComponents (VectorImpl One b r)
                       (VectorImpl One b r) where
  components = itraversed


instance (UV.Unbox r) => HasComponents (VectorImpl (Large d) Unboxed r)
                                       (VectorImpl (Large d) Boxed s) where
  components = conjoined traverse' (itraverse' . indexed)
    where
      traverse'     :: Applicative f => (r -> f s)
                    -> VectorImpl (Large d) Unboxed r
                    -> f (VectorImpl (Large d) Boxed s)
      traverse' f (coerce -> v) = coerce . V.fromListN (UV.length v)
                               <$> traverse f (UV.toList v)

      itraverse' :: Applicative f => (Int -> r -> f s)
                 -> VectorImpl (Large d) Unboxed r
                 -> f (VectorImpl (Large d) Boxed s)
      itraverse' f (coerce -> v) = coerce . V.fromListN (UV.length v)
                                <$> itraverse f (UV.toList v)

instance (UV.Unbox r, UV.Unbox s) => HasComponents (VectorImpl (Large d) Unboxed r)
                                                   (VectorImpl (Large d) Unboxed s) where
  components = conjoined traverse' (itraverse' . indexed)
    where
      traverse'     :: Applicative f => (r -> f s)
                    -> VectorImpl (Large d) Unboxed r
                    -> f (VectorImpl (Large d) Unboxed s)
      traverse' f (coerce -> v) = coerce . UV.fromListN (UV.length v)
                               <$> traverse f (UV.toList v)

      itraverse' :: Applicative f => (Int -> r -> f s)
                 -> VectorImpl (Large d) Unboxed r
                 -> f (VectorImpl (Large d) Unboxed s)
      itraverse' f (coerce -> v) = coerce . UV.fromListN (UV.length v)
                                <$> itraverse f (UV.toList v)


--------------------------------------------------------------------------------
-- * Vector_ instances

instance Vector_ (VectorImpl Zero b r) 0 r where
  vectorFromList = \case
    [] -> Just $ V_0 ()
    _  -> Nothing
  {-# INLINE vectorFromList #-}

instance Vector_ (VectorImpl One b r) 1 r where
  vectorFromList = \case
    [x] -> Just $ coerce x
    _   -> Nothing
  {-# INLINE vectorFromList #-}

instance Vector_ (VectorImpl Two Boxed r) 2 r where
  vectorFromList = \case
    [x,y] -> Just $ V_2 (L2.V2 x y)
    _     -> Nothing
  {-# INLINE vectorFromList #-}

instance Vector_ (VectorImpl Three Boxed r) 3 r where
  vectorFromList = \case
    [x,y,z] -> Just $ V_3 (L3.V3 x y z)
    _       -> Nothing
  {-# INLINE vectorFromList #-}

instance Vector_ (VectorImpl Four Boxed r) 4 r where
  vectorFromList = \case
    [x,y,z,w] -> Just $ V_4 (L4.V4 x y z w)
    _         -> Nothing
  {-# INLINE vectorFromList #-}

instance (KnownNat d) => Vector_ (VectorImpl (Large d) Boxed r) d r where
  vectorFromList xs = let d = fromInteger $ natVal $ Proxy @d
                          v = V.fromList xs
                        in if length v == d then Just (coerce v) else Nothing
  {-# INLINE vectorFromList #-}
  -- FIXME: rewrite rule for vector
instance (KnownNat d, UV.Unbox r) => Vector_ (VectorImpl (Large d) Unboxed r) d r where
  vectorFromList xs = let d = fromInteger $ natVal $ Proxy @d
                          v = UV.fromList xs
                        in if UV.length v == d then Just (coerce v) else Nothing
  {-# INLINE vectorFromList #-}
  -- FIXME: rewrite rule for vector


instance ( Applicative (VectorImpl o Boxed)
         , TraversableWithIndex Int (VectorImpl o Boxed)
         ) => Additive_ (VectorImpl o Boxed r) where
  zero = pure 0
  liftU2 = liftA2
  liftI2 = liftA2
  {-# SPECIALIZE instance Additive_ (VectorImpl Zero Boxed r)      #-}
  {-# SPECIALIZE instance Additive_ (VectorImpl One Boxed r)       #-}
  {-# SPECIALIZE instance Additive_ (VectorImpl Two Boxed r)       #-}
  {-# SPECIALIZE instance Additive_ (VectorImpl Three Boxed r)     #-}
  {-# SPECIALIZE instance Additive_ (VectorImpl Four Boxed r)      #-}
  {-# SPECIALIZE instance Additive_ (VectorImpl (Large d) Boxed r) #-}


instance ( Applicative (VectorImpl o Boxed)
         , TraversableWithIndex Int (VectorImpl o Boxed)
         ) => Metric_ (VectorImpl o Boxed r) where
  {-# SPECIALIZE instance Metric_ (VectorImpl Zero Boxed r)      #-}
  {-# SPECIALIZE instance Metric_ (VectorImpl One Boxed r)       #-}
  {-# SPECIALIZE instance Metric_ (VectorImpl Two Boxed r)       #-}
  {-# SPECIALIZE instance Metric_ (VectorImpl Three Boxed r)     #-}
  {-# SPECIALIZE instance Metric_ (VectorImpl Four Boxed r)      #-}
  {-# SPECIALIZE instance Metric_ (VectorImpl (Large d) Boxed r) #-}


instance (UV.Unbox r, KnownNat d
         ) => Additive_ (VectorImpl (Large d) Unboxed r) where
  zero = let d = fromInteger $ natVal $ Proxy @d
         in coerce $ UV.replicate d 0
  liftU2 f (coerce -> u) (coerce -> v) = coerce $ UV.zipWith f u v
  liftI2 f (coerce -> u) (coerce -> v) = coerce $ UV.zipWith f u v


--------------------------------------------------------------------------------

instance ( KnownNat d
         , Vector_ (VectorImpl o b r) d r
         , Show r) => Show (VectorImpl o b r) where
  showsPrec = showsPrecVec

instance ( KnownNat d
         , Vector_ (VectorImpl o b r) d r
         , Read r) => Read (VectorImpl o b r) where
  readPrec = readPrecVec
  readListPrec = readListPrecDefault
