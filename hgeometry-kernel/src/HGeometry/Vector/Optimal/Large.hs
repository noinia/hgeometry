 module HGeometry.Vector.Optimal.Large
   ( LargeVector(..)
   , FallBack(..)
   ) where

import           Control.DeepSeq
import           Control.Lens
import           Data.Coerce
import           Data.Kind (Type)
import           Data.Proxy
import qualified Data.Vector as V
import qualified Data.Vector.Generic as GV
import qualified Data.Vector.Generic.Mutable as GMV
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as UMV
import           GHC.Generics
import           GHC.TypeLits
import           HGeometry.Properties
import           HGeometry.Vector.Class

--------------------------------------------------------------------------------

-- | Large unboxed vectors of an unboxable type
type LargeVector :: Nat -> Type -> Type
newtype LargeVector d r = LargeVector (UV.Vector r)
  deriving (Eq,Ord,NFData,Generic)

type instance Dimension (LargeVector d r) = d
type instance NumType (LargeVector d r)   = r
type instance IxValue (LargeVector d r)   = r
type instance Index   (LargeVector d r)   = Int

instance Ixed (LargeVector d r) where
  ix i f v = ix i f (coerce v)
  {-# INLINE ix #-}

instance (UV.Unbox r, UV.Unbox s) => HasComponents (LargeVector d r)
                                                   (LargeVector d s) where
  components = conjoined traverse' (itraverse' . indexed)
    where
      traverse'     :: Applicative f => (r -> f s)
                    -> LargeVector d r
                    -> f (LargeVector d s)
      traverse' f (coerce -> v) = coerce . UV.fromListN (UV.length v)
                               <$> traverse f (UV.toList v)

      itraverse' :: Applicative f => (Int -> r -> f s)
                 -> LargeVector d r
                 -> f (LargeVector d s)
      itraverse' f (coerce -> v) = coerce . UV.fromListN (UV.length v)
                                <$> itraverse f (UV.toList v)


instance (KnownNat d, UV.Unbox r) => Vector_ (LargeVector d r) d r where
  vectorFromList xs = let d = fromInteger $ natVal $ Proxy @d
                          v = UV.fromList xs
                        in if UV.length v == d then Just (coerce v) else Nothing
  {-# INLINE vectorFromList #-}
  -- FIXME: rewrite rule for vector



--------------------------------------------------------------------------------


-- | Newtype wrapper around r, that lets us implement an "unboxed" vector by a boxed vector.
newtype FallBack r = FallBack r

-- | Fall back to boxed vectors
newtype instance UMV.MVector s (FallBack r) = MV_FallBack (MV.MVector s r)

newtype instance UV.Vector     (FallBack r) = V_FallBack  (V.Vector     r)

instance GMV.MVector UMV.MVector (FallBack r) where
  basicLength (MV_FallBack v) = GMV.basicLength v
  {-# INLINE basicLength #-}
  basicUnsafeSlice s l v = GMV.basicUnsafeSlice s l (coerce v)
  {-# INLINE basicUnsafeSlice #-}
  basicOverlaps (MV_FallBack v) (MV_FallBack v') = GMV.basicOverlaps v v'
  {-# INLINE basicOverlaps #-}
  basicUnsafeNew n = MV_FallBack <$> GMV.basicUnsafeNew n
  {-# INLINE basicUnsafeNew #-}
  basicInitialize = GMV.basicInitialize
  {-# INLINE basicInitialize#-}
  basicUnsafeRead (MV_FallBack v) i = coerce <$> GMV.basicUnsafeRead v i
  {-# INLINE basicUnsafeRead #-}
  basicUnsafeWrite (MV_FallBack v) i (FallBack r) = GMV.basicUnsafeWrite v i r
  {-# INLINE basicUnsafeWrite #-}


instance GV.Vector UV.Vector (FallBack r) where
  basicUnsafeFreeze (MV_FallBack mv) = V_FallBack <$> GV.basicUnsafeFreeze mv
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeThaw (V_FallBack v) = MV_FallBack <$> GV.basicUnsafeThaw v
  {-# INLINE basicUnsafeThaw #-}
  basicLength (V_FallBack v) = GV.basicLength v
  {-# INLINE basicLength #-}
  basicUnsafeSlice s l (V_FallBack v) = V_FallBack $ GV.basicUnsafeSlice s l v
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeIndexM (V_FallBack v) i = coerce <$> GV.basicUnsafeIndexM v i
  {-# INLINE basicUnsafeIndexM #-}

instance UV.Unbox (FallBack r)
