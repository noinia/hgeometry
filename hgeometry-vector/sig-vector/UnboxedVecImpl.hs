{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}
module UnboxedVecImpl
  (
  ) where

import           Control.Lens
import           D
import           Data.Proxy
import qualified Data.Vector.Generic as GV
import           Data.Vector.Generic.Mutable (MVector(basicInitialize))
import qualified Data.Vector.Generic.Mutable as GMV
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as UMV
import           GHC.TypeLits (natVal)
import           R
import           Vector

--------------------------------------------------------------------------------
-- * unboxed vectors

-- | elements of the vector are stored consecutively
newtype instance UMV.MVector s Vector = MV_VectorD (UMV.MVector s R)
newtype instance UV.Vector     Vector = V_VectorD  (UV.Vector     R)

d :: Int
d = fromInteger $ natVal (Proxy @D)

instance ( GMV.MVector UMV.MVector R
         ) => GMV.MVector UMV.MVector Vector where

  basicLength (MV_VectorD v) = GMV.basicLength v `div` d
  {-# INLINE basicLength #-}
  basicUnsafeSlice s l (MV_VectorD v) = MV_VectorD $ GMV.basicUnsafeSlice (d*s) (d*l) v
  {-# INLINE basicUnsafeSlice #-}
  basicOverlaps  (MV_VectorD v) (MV_VectorD v') = GMV.basicOverlaps v v'
  {-# INLINE basicOverlaps #-}
  basicUnsafeNew n = MV_VectorD <$> GMV.basicUnsafeNew (d*n)
  {-# INLINE basicUnsafeNew #-}
  basicInitialize (MV_VectorD v) = GMV.basicInitialize v
  {-# INLINE basicInitialize#-}
  basicUnsafeRead (MV_VectorD v) i = generateA $ \j -> GMV.basicUnsafeRead v (d*i+j)
  {-# INLINE basicUnsafeRead #-}
  basicUnsafeWrite (MV_VectorD v) i w = imapMOf_ components f w
    where
      f j x = GMV.basicUnsafeWrite v (d*i+j) x
  {-# INLINE basicUnsafeWrite #-}


instance ( GV.Vector UV.Vector R
         ) => GV.Vector UV.Vector Vector where

  basicUnsafeFreeze (MV_VectorD mv) = V_VectorD <$> GV.basicUnsafeFreeze mv
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeThaw (V_VectorD v) = MV_VectorD <$> GV.basicUnsafeThaw v
  {-# INLINE basicUnsafeThaw #-}
  basicLength (V_VectorD v) = GV.basicLength v `div` d
  {-# INLINE basicLength #-}
  basicUnsafeSlice s l (V_VectorD v) = V_VectorD $ GV.basicUnsafeSlice (d*s) (d*l) v
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeIndexM (V_VectorD v) i = generateA $ \j -> GV.basicUnsafeIndexM v (d*i+j)
  {-# INLINE basicUnsafeIndexM #-}

instance ( UV.Unbox R
         ) => UV.Unbox Vector where
