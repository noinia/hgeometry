{-# LANGUAGE UndecidableInstances #-}
module Optimal.Internal
  ( Vector(MkVector, Vector1, Vector2, Vector3, Vector4)
  , VectorFamily, VectorFamily'
  , OptVector_, OptCVector_
  ) where

import           Control.Applicative
import           Control.DeepSeq
import           Control.Lens
import           Data.Coerce
import           Data.Kind
import           GHC.Generics
import           GHC.TypeLits
import           HGeometry.Properties
import           HGeometry.Vector.Class
import           HGeometry.Vector.Helper
-- import qualified HGeometry.Vector.Optimal.Large as Large
import qualified Optimal.V1 as V1
-- import qualified HGeometry.Vector.Optimal.V2 as V2
-- import qualified HGeometry.Vector.Optimal.V3 as V3
-- import qualified HGeometry.Vector.Optimal.V4 as V4
import           System.Random (Random (..))
import           System.Random.Stateful (UniformRange(..), Uniform(..))
import           Text.Read (Read (..), readListPrecDefault)
import qualified Data.Vector.Generic as GV
import qualified Data.Vector.Generic.Mutable as GMV
import qualified Data.Vector.Unboxed.Mutable as UMV
import qualified Data.Vector.Unboxed as UV

--------------------------------------------------------------------------------

-- | d-dimensional vectors that pick a specialized implementation
-- depending on the dimension and the numtype.
type    Vector     :: Nat -> Type -> Type
newtype Vector d r = MkVector (VectorFamily' d r)

type instance Dimension (Vector d r) = d
type instance NumType  (Vector d r)  = r
type instance IxValue  (Vector d r)  = r
type instance Index    (Vector d r)  = Int


-- | The type family to specialize vector implementations. For 0 and 1
-- dimensions we already decide how to implement the vector, since
-- there is a unique optimal representation. For the rest, we defer th
-- eimplementation type to the VectorFamily type family defined below.
--
--
type VectorFamily' :: Nat -> Type -> Type
type family VectorFamily' d r where
  VectorFamily' 0 r  = Const () r  -- this does not store any r,s so no need to specialize
  VectorFamily' 1 r  = V1.V1 r     -- this is just a newtype, so no need to choose
  VectorFamily' d r  = VectorFamily d r

-- | The type family to specialize vector implementations.
type VectorFamily :: Nat -> Type -> Type
type family VectorFamily d r

-- | Shorthand for the constraint that our optimal vector is a vector
type OptVector_ d r = Vector_ (VectorFamily' d r) d r

-- | Shorthand for the constraint that our optimal vector is a constructable vector
type OptCVector_ d r = ConstructableVector_ (VectorFamily d r) d r

---------------------------------------------------------------------------------
-- * Constructors for Small vectors

-- | Construct a 1 dimensional vector
pattern Vector1   :: r -> Vector 1 r
pattern Vector1 x = MkVector (V1.Vector1 x)

-- | Construct a 2 dimensional vector
pattern Vector2     :: forall r. OptCVector_ 2 r
                    => r -> r -> Vector 2 r
pattern Vector2 x y <- MkVector (Vector2_ x y)
  where
    Vector2 x y = MkVector $ mkVector @(VectorFamily 2 r) x y

-- | Construct a 3 dimensional vector
pattern Vector3       :: forall r. OptCVector_ 3 r
                      => r -> r -> r -> Vector 3 r
pattern Vector3 x y z <- MkVector (Vector3_ x y z)
  where
    Vector3 x y z = MkVector $ mkVector @(VectorFamily 3 r) x y z

-- | Construct a 4 dimensional vector
pattern Vector4         :: forall r. OptCVector_ 4 r
                        => r -> r -> r -> r -> Vector 4 r
pattern Vector4 x y z w <- MkVector (Vector4_ x y z w)
  where
    Vector4 x y z w = MkVector $ mkVector @(VectorFamily 4 r) x y w z

-- --------------------------------------------------------------------------------

deriving newtype instance Eq (VectorFamily' d r)      => Eq (Vector d r)
deriving newtype instance Ord (VectorFamily' d r)     => Ord (Vector d r)
deriving newtype instance NFData (VectorFamily' d r)  => NFData (Vector d r)
deriving newtype instance Generic (VectorFamily' d r) => Generic (Vector d r)

instance ( Ixed (VectorFamily' d r)
         , IxValue (VectorFamily' d r) ~ r
         , Index (VectorFamily' d r) ~ Int
         ) => Ixed (Vector d r) where
  ix i f (MkVector v) = MkVector <$> ix i f v
  {-# INLINE ix #-}

instance {-# OVERLAPPING #-}
         ( HasComponents (VectorFamily' d r) (VectorFamily' d s)
         , IxValue (VectorFamily' d r) ~ r
         , IxValue (VectorFamily' d s) ~ s)
      => HasComponents (Vector d r)     (Vector d s) where
  components f (MkVector v) = MkVector <$> components f v
  {-# INLINE components #-}

instance ( HasComponents (VectorFamily' d r) v
         , IxValue (VectorFamily' d r) ~ r
         ) => HasComponents (Vector d r) v where
  components f (MkVector v) = components f v
  {-# INLINE components #-}


instance Vector_ (VectorFamily' d r) d r => Vector_ (Vector d r) d r where
  vectorFromList = fmap MkVector . vectorFromList @(VectorFamily' d r)

deriving newtype instance ( Additive_ (VectorFamily' d r)
                          , IxValue (VectorFamily' d r) ~ r
                          ) => Additive_ (Vector d r)
deriving newtype instance ( Metric_ (VectorFamily' d r)
                          , IxValue (VectorFamily' d r) ~ r
                          ) => Metric_   (Vector d r)



instance ( Show r, KnownNat d, Vector_ (Vector d r) d r
         ) => Show (Vector d r) where
  showsPrec = showsPrecVec
instance ( Read r, KnownNat d, Vector_ (Vector d r) d r
         ) => Read (Vector d r) where
  readPrec = readPrecVec
  readListPrec = readListPrecDefault


deriving newtype instance Random (VectorFamily' d r) => Random (Vector d r)

instance UniformRange (VectorFamily' d r) => UniformRange (Vector d r) where
  uniformRM rng gen = MkVector <$> uniformRM (coerce rng) gen

instance Uniform (VectorFamily' d r) => Uniform (Vector d r) where
  uniformM gen = MkVector <$> uniformM gen

--------------------------------------------------------------------------------
-- Vector instances, so we can store a Vector (from the vector
-- package) of Vectors (our vectors) efficiently

newtype instance UMV.MVector s (Vector d r) = MV_Vec (UMV.MVector s (VectorFamily' d r))
newtype instance UV.Vector     (Vector d r) = V_Vec (UV.Vector     (VectorFamily' d r))

instance GMV.MVector UMV.MVector (VectorFamily' d r)
      => GMV.MVector UMV.MVector (Vector d r) where
  basicLength (MV_Vec v) = GMV.basicLength v
  {-# INLINE basicLength #-}
  basicUnsafeSlice s l (MV_Vec v) = MV_Vec $ GMV.basicUnsafeSlice s l v
  {-# INLINE basicUnsafeSlice #-}
  basicOverlaps  (MV_Vec v) (MV_Vec v') = GMV.basicOverlaps v v'
  {-# INLINE basicOverlaps #-}
  basicUnsafeNew n = MV_Vec <$> GMV.basicUnsafeNew n
  {-# INLINE basicUnsafeNew #-}
  basicInitialize (MV_Vec v) = GMV.basicInitialize v
  {-# INLINE basicInitialize#-}
  basicUnsafeRead (MV_Vec v) i = MkVector <$> GMV.basicUnsafeRead v i
  {-# INLINE basicUnsafeRead #-}
  basicUnsafeWrite (MV_Vec v) i (MkVector vf) = GMV.basicUnsafeWrite v i vf

  {-# INLINE basicUnsafeWrite #-}



instance GV.Vector UV.Vector (VectorFamily' d r)
      => GV.Vector UV.Vector (Vector d r) where

  basicUnsafeFreeze (MV_Vec mv) = V_Vec <$> GV.basicUnsafeFreeze mv
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeThaw (V_Vec v) = MV_Vec <$> GV.basicUnsafeThaw v
  {-# INLINE basicUnsafeThaw #-}
  basicLength (V_Vec v) = GV.basicLength v
  {-# INLINE basicLength #-}
  basicUnsafeSlice s l (V_Vec v) = V_Vec $ GV.basicUnsafeSlice s l v
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeIndexM (V_Vec v) i = MkVector <$> GV.basicUnsafeIndexM v i
  {-# INLINE basicUnsafeIndexM #-}

instance UV.Unbox (VectorFamily' d r) => UV.Unbox (Vector d r)
