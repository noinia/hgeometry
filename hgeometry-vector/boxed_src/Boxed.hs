{-# LANGUAGE UndecidableInstances #-}
module Boxed
  ( Vector, pattern Vector1, pattern Vector2, pattern Vector3, pattern Vector4
  ) where

import           Control.Applicative
import           Control.DeepSeq
import           Control.Lens
import           Data.Coerce
import           Data.Functor.Classes
import           Data.Kind
import           Data.Proxy
import qualified Data.Vector as V
import           GHC.Generics
import           GHC.TypeLits
import           HGeometry.Properties
import           HGeometry.Vector.Class
import qualified Linear.V2 as L2
import qualified Linear.V3 as L3
import qualified Linear.V4 as L4
import           System.Random.Stateful (Uniform(..), UniformRange(..))

--------------------------------------------------------------------------------

-- | d-dimensional vectors that pick a specialized implementation for sizes up to 4.
type Vector d r = VectorImpl (Choose d) r


--------------------------------------------------------------------------------
-- * Convenience "constructors"

-- | Constant sized vector with d elements.
-- pattern Vector   :: VectorFamilyF (Peano d) r -> Vector d r
-- pattern Vector v = MKVector (VectorFamily v)
-- {-# COMPLETE Vector #-}

-- | Constant sized vector with 1 element.
pattern Vector1   :: r -> Vector 1 r
pattern Vector1 x = (V_1 (Identity x))
{-# COMPLETE Vector1 #-}

-- | Constant sized vector with 2 elements.
pattern Vector2     :: r -> r -> Vector 2 r
pattern Vector2 x y = (V_2 (L2.V2 x y))
{-# COMPLETE Vector2 #-}

-- | Constant sized vector with 3 elements.
pattern Vector3        :: r -> r -> r -> Vector 3 r
pattern Vector3 x y z  = (V_3 (L3.V3 x y z))
{-# COMPLETE Vector3 #-}

-- | Constant sized vector with 4 elements.
pattern Vector4         :: r -> r -> r -> r -> Vector 4 r
pattern Vector4 x y z w = (V_4 (L4.V4 x y z w))
{-# COMPLETE Vector4 #-}

-- -- | Constant sized vector with 4 elements.
-- pattern Vector   :: V.Vector r -> Vector d r
-- pattern Vector v = (V_Large v)
-- {-# COMPLETE Vector #-}


--------------------------------------------------------------------------------

type family Choose d :: (Optimize Nat) where
  Choose 0 = Zero
  Choose 1 = One
  Choose 2 = Two
  Choose 3 = Three
  Choose 4 = Four
  Choose d = Large d

data Optimize d = Zero | One | Two | Three | Four | Large d

type VectorImpl :: Optimize Nat -> Type -> Type
data family VectorImpl o r

newtype instance VectorImpl Zero r      = V_0 ()
  deriving (Eq,Ord,NFData,Generic,Functor,Foldable,Traversable)
newtype instance VectorImpl One r       = V_1 (Identity r)
  deriving (Eq,Eq1,Ord,Ord1,NFData,Generic,Functor,Foldable,Traversable,Applicative)
newtype instance VectorImpl Two r       = V_2 (L2.V2 r)
  deriving (Eq,Eq1,Ord,Ord1,NFData,Generic,Functor,Foldable,Traversable,Applicative)
newtype instance VectorImpl Three r     = V_3 (L3.V3 r)
  deriving (Eq,Eq1,Ord,Ord1,NFData,Generic,Functor,Foldable,Traversable,Applicative)
newtype instance VectorImpl Four r      = V_4 (L4.V4 r)
  deriving (Eq,Eq1,Ord,Ord1,NFData,Generic,Functor,Foldable,Traversable,Applicative)
newtype instance VectorImpl (Large d) r = V_Large (V.Vector r)
  deriving (Eq,Eq1,Ord,Ord1,NFData,Generic,Functor,Foldable,Traversable,Applicative)
-- ideally we would even choose unboxed based on r here.

type instance NumType (VectorImpl o r) = r
type instance IxValue (VectorImpl o r) = r
type instance Index   (VectorImpl o r) = Int

type instance Dimension (VectorImpl Zero r     ) = 0
type instance Dimension (VectorImpl One r      ) = 1
type instance Dimension (VectorImpl Two r      ) = 2
type instance Dimension (VectorImpl Three r    ) = 3
type instance Dimension (VectorImpl Four r     ) = 4
type instance Dimension (VectorImpl (Large d) r) = d

instance Eq1 (VectorImpl Zero) where
  liftEq _ _ _ = True

instance Ord1 (VectorImpl Zero) where
  liftCompare _ _ _ = EQ

instance Applicative (VectorImpl Zero) where
  pure _ = V_0 ()
  _ <*> _ = V_0 ()

instance Ixed (VectorImpl Zero r     ) where
  ix _ _ = pure
instance Ixed (VectorImpl One r      ) where
  ix i f v@(V_1 (Identity x)) = case i of
                       0 -> coerce <$> f x
                       _ -> pure v
  {-# INLINE ix #-}


instance Ixed (VectorImpl Two r      ) where
  ix i f v = ix i f (coerce v)
  {-# INLINE ix #-}

instance Ixed (VectorImpl Three r    ) where
  ix i f v = ix i f (coerce v)
  {-# INLINE ix #-}

instance Ixed (VectorImpl Four r     ) where
  ix i f v = ix i f (coerce v)
  {-# INLINE ix #-}

instance Ixed (VectorImpl (Large d) r) where
  ix i f v = ix i f (coerce v)
  {-# INLINE ix #-}

instance FunctorWithIndex Int (VectorImpl Zero)
instance FunctorWithIndex Int (VectorImpl One)
instance FunctorWithIndex Int (VectorImpl Two)
instance FunctorWithIndex Int (VectorImpl Three)
instance FunctorWithIndex Int (VectorImpl Four)
instance FunctorWithIndex Int (VectorImpl (Large d))

instance FoldableWithIndex Int (VectorImpl Zero)
instance FoldableWithIndex Int (VectorImpl One)
instance FoldableWithIndex Int (VectorImpl Two)
instance FoldableWithIndex Int (VectorImpl Three)
instance FoldableWithIndex Int (VectorImpl Four)
instance FoldableWithIndex Int (VectorImpl (Large d))

instance TraversableWithIndex Int (VectorImpl Zero)
instance TraversableWithIndex Int (VectorImpl One)
instance TraversableWithIndex Int (VectorImpl Two)
instance TraversableWithIndex Int (VectorImpl Three)
instance TraversableWithIndex Int (VectorImpl Four)
instance TraversableWithIndex Int (VectorImpl (Large d))

instance TraversableWithIndex Int (VectorImpl o)
         => HasComponents (VectorImpl o r) (VectorImpl o s) where
  {-# SPECIALIZE instance HasComponents (VectorImpl Zero r)      (VectorImpl Zero s) #-}
  {-# SPECIALIZE instance HasComponents (VectorImpl One r)       (VectorImpl One s) #-}
  {-# SPECIALIZE instance HasComponents (VectorImpl Two r)       (VectorImpl Two s) #-}
  {-# SPECIALIZE instance HasComponents (VectorImpl Three r)     (VectorImpl Three s) #-}
  {-# SPECIALIZE instance HasComponents (VectorImpl Four r)      (VectorImpl Four s) #-}
  {-# SPECIALIZE instance HasComponents (VectorImpl (Large d) r) (VectorImpl (Large d) s) #-}
  components = itraversed

instance Vector_ (VectorImpl Zero r) 0 r where
  vectorFromList = \case
    [] -> Just $ V_0 ()
    _  -> Nothing
  {-# INLINE vectorFromList #-}

instance Vector_ (VectorImpl One r) 1 r where
  vectorFromList = \case
    [x] -> Just $ Vector1 x
    _   -> Nothing
  {-# INLINE vectorFromList #-}

instance Vector_ (VectorImpl Two r) 2 r where
  vectorFromList = \case
    [x,y] -> Just $ Vector2 x y
    _     -> Nothing
  {-# INLINE vectorFromList #-}

instance Vector_ (VectorImpl Three r) 3 r where
  vectorFromList = \case
    [x,y,z] -> Just $ Vector3 x y z
    _       -> Nothing
  {-# INLINE vectorFromList #-}

instance Vector_ (VectorImpl Four r) 4 r where
  vectorFromList = \case
    [x,y,z,w] -> Just $ Vector4 x y z w
    _         -> Nothing
  {-# INLINE vectorFromList #-}

instance KnownNat d => Vector_ (VectorImpl (Large d) r) d r where
  vectorFromList xs = let d = fromInteger $ natVal $ Proxy @d
                          v = V.fromList xs
                        in if length v == d then Just (coerce v) else Nothing
  {-# INLINE vectorFromList #-}
  -- FIXME: rewrite rule for vector

instance ConstructableVector_ (VectorImpl Zero r) 0 r where
  mkVector = V_0 ()
instance ConstructableVector_ (VectorImpl One r) 1 r where
  mkVector = Vector1
instance ConstructableVector_ (VectorImpl Two r) 2 r where
  mkVector = Vector2
instance ConstructableVector_ (VectorImpl Three r) 3 r where
  mkVector = Vector3
instance ConstructableVector_ (VectorImpl Four r) 4 r where
  mkVector = Vector4

-- instance KnownNat d => ConstructableVector_ (VectorImpl (Large d) r) d r where
--   mkVector -- requires arity d



instance ( Applicative (VectorImpl o)
         , TraversableWithIndex Int (VectorImpl o)
         ) => Additive_ (VectorImpl o r) where
  zero = pure 0
  liftU2 = liftA2
  liftI2 = liftA2
  {-# SPECIALIZE instance Additive_ (VectorImpl Zero r)      #-}
  {-# SPECIALIZE instance Additive_ (VectorImpl One r)       #-}
  {-# SPECIALIZE instance Additive_ (VectorImpl Two r)       #-}
  {-# SPECIALIZE instance Additive_ (VectorImpl Three r)     #-}
  {-# SPECIALIZE instance Additive_ (VectorImpl Four r)      #-}
  {-# SPECIALIZE instance Additive_ (VectorImpl (Large d) r) #-}

instance ( Applicative (VectorImpl o)
         , TraversableWithIndex Int (VectorImpl o)
         ) => Metric_ (VectorImpl o r) where
  {-# SPECIALIZE instance Metric_ (VectorImpl Zero r)      #-}
  {-# SPECIALIZE instance Metric_ (VectorImpl One r)       #-}
  {-# SPECIALIZE instance Metric_ (VectorImpl Two r)       #-}
  {-# SPECIALIZE instance Metric_ (VectorImpl Three r)     #-}
  {-# SPECIALIZE instance Metric_ (VectorImpl Four r)      #-}
  {-# SPECIALIZE instance Metric_ (VectorImpl (Large d) r) #-}

instance Uniform (VectorImpl Zero r)
instance Uniform r => Uniform (VectorImpl One r) where
  uniformM g = Vector1 <$> uniformM g
instance Uniform r => Uniform (VectorImpl Two r) where
  uniformM g = Vector2 <$> uniformM g <*> uniformM g
instance Uniform r => Uniform (VectorImpl Three r) where
  uniformM g = Vector3 <$> uniformM g <*> uniformM g <*> uniformM g
instance Uniform r => Uniform (VectorImpl Four r) where
  uniformM g = Vector4 <$> uniformM g <*> uniformM g <*> uniformM g <*> uniformM g
instance Uniform r => Uniform (VectorImpl (Large d) r) where
  uniformM g = sequenceA $ pure (uniformM g)

instance UniformRange r => UniformRange (VectorImpl One r) where
  uniformRM (Vector1 low, Vector1 high) g = Vector1 <$> uniformRM (low, high) g

instance UniformRange r => UniformRange (VectorImpl Two r) where
  uniformRM (Vector2 lx ly, Vector2 hx hy) g = Vector2 <$> uniformRM (lx, hx) g
                                                       <*> uniformRM (ly, hy) g

instance UniformRange r => UniformRange (VectorImpl Three r) where
  uniformRM (Vector3 lx ly lz, Vector3 hx hy hz) g = Vector3 <$> uniformRM (lx, hx) g
                                                             <*> uniformRM (ly, hy) g
                                                             <*> uniformRM (lz, hz) g

instance UniformRange r => UniformRange (VectorImpl Four r) where
  uniformRM (Vector4 lx ly lz lw, Vector4 hx hy hz hw) g = Vector4 <$> uniformRM (lx, hx) g
                                                                   <*> uniformRM (ly, hy) g
                                                                   <*> uniformRM (lz, hz) g
                                                                   <*> uniformRM (lw, hw) g

instance UniformRange r => UniformRange (VectorImpl (Large d) r) where
  uniformRM (lv, hv) g = sequenceA $ liftA2 (\lx hx -> uniformRM (lx,hx) g) lv hv
