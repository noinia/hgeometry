{-# LANGUAGE UndecidableInstances #-}
module Optimal.Wrap
  ( WrapVector(..)
  ) where

import           Control.DeepSeq
import           Control.Lens
import           Data.Coerce
import qualified Data.Vector.Generic as GV
import qualified Data.Vector.Generic.Mutable as GMV
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as UMV
import           GHC.Generics
import           HGeometry.Properties
import           HGeometry.Vector.Class
import           Optimal.Internal

--------------------------------------------------------------------------------

-- | 'WrapVector d orig wrapped' implements an vector for type
-- 'wrapped' by using the optimal underlying Vector for type 'orig'.
newtype WrapVector d orig wrapped = WrapVector (Vector d orig) deriving (Generic)

type instance Dimension (WrapVector d orig wrapped) = d
type instance IxValue   (WrapVector d orig wrapped) = wrapped
type instance Index     (WrapVector d orig wrapped) = Int


deriving newtype instance Eq (Vector d orig)     => Eq (WrapVector d orig wrapped)
deriving newtype instance Ord (Vector d orig)    => Ord (WrapVector d orig wrapped)
deriving newtype instance NFData (Vector d orig) => NFData (WrapVector d orig wrapped)

instance Wrapped (WrapVector d orig wrapped)
instance (t ~ WrapVector d' orig' wrapped') => Rewrapped (WrapVector d orig wrapped) t

instance ( Coercible orig wrapped
         , Vector_ (Vector d orig) d orig
         ) => Ixed (WrapVector d orig wrapped) where
  ix i = _Wrapped.ix i.coerced

instance ( Coercible orig wrapped
         , Vector_ (Vector d orig) d orig
         ) => HasComponents (WrapVector d orig wrapped) (WrapVector d orig wrapped) where
  components = _Wrapped.components.coerced

instance ( Coercible orig wrapped
         , Vector_ (Vector d orig) d orig
         ) => Vector_ (WrapVector d orig wrapped) d wrapped where
  vectorFromList = coerce . vectorFromList @(Vector d orig) . coerce

instance ( Coercible orig wrapped
         , Coercible (ConstructVector (Vector     d orig)         d)
                     (ConstructVector (WrapVector d orig wrapped) d)
           -- somehow the above should be true, since functions are
           -- coercible. But somehow GHC doesn't see that.

         , ConstructableVector_ (Vector d orig) d orig
         ) => ConstructableVector_ (WrapVector d orig wrapped) d wrapped where
  mkVector = coerce @(ConstructVector (Vector     d orig)         d)
                    @(ConstructVector (WrapVector d orig wrapped) d)
                    (mkVector @(Vector d orig) @d)

instance ( Coercible orig wrapped
         , Vector_ (Vector d orig) d orig
         , Additive_ (Vector d orig)
         , Num orig
         ) => Additive_ (WrapVector d orig wrapped) where
  zero = WrapVector zero
  liftU2 f (WrapVector u) (WrapVector v) = WrapVector $ liftU2 (coerce f) u v
  liftI2 f (WrapVector u) (WrapVector v) = WrapVector $ liftI2 (coerce f) u v

instance ( Coercible orig wrapped
         , Vector_ (Vector d orig) d orig
         , Additive_ (Vector d orig)
         , Num orig
         ) => Metric_ (WrapVector d orig wrapped)

-- type F :: Type -> Type -> Nat -> Type
-- type family F a result d where
--   F a result 0 = result
--   F a result d = a -> F a result (d-1)

-- myAF :: forall a result d. F a result d
-- myAF = undefined

-- myBF :: forall a b result result' d.
--         ( Coercible a b
--         , Coercible result result'
--         , Coercible (F a result d) (F b result' d) -- why do I need to provide this constraint?
--         ) => F b result' d
-- myBF = coerce @(F a result d) @(F b result' d) (myAF @a @result @d)


newtype instance UMV.MVector s (WrapVector d orig wrapped) =
  MV_Wrap (UMV.MVector s (Vector d orig))
newtype instance UV.Vector (WrapVector d orig wrapped) =
  V_Wrap (UV.Vector (Vector d orig))

instance ( GMV.MVector UMV.MVector (Vector d orig)
         , Coercible orig wrapped
         ) => GMV.MVector UMV.MVector (WrapVector d orig wrapped) where
  basicLength (MV_Wrap v) = GMV.basicLength v
  {-# INLINE basicLength #-}
  basicUnsafeSlice s l (MV_Wrap v) = MV_Wrap $ GMV.basicUnsafeSlice s l v
  {-# INLINE basicUnsafeSlice #-}
  basicOverlaps  (MV_Wrap v) (MV_Wrap v') = GMV.basicOverlaps v v'
  {-# INLINE basicOverlaps #-}
  basicUnsafeNew n = MV_Wrap <$> GMV.basicUnsafeNew n
  {-# INLINE basicUnsafeNew #-}
  basicInitialize (MV_Wrap v) = GMV.basicInitialize v
  {-# INLINE basicInitialize#-}
  basicUnsafeRead (MV_Wrap v) i = coerce <$> GMV.basicUnsafeRead v i
  {-# INLINE basicUnsafeRead #-}
  basicUnsafeWrite (MV_Wrap v) i (WrapVector x) = GMV.basicUnsafeWrite v i x
  {-# INLINE basicUnsafeWrite #-}

instance ( GV.Vector UV.Vector (Vector d orig)
         , Coercible orig wrapped
         ) => GV.Vector UV.Vector (WrapVector d orig wrapped) where

  basicUnsafeFreeze (MV_Wrap mv) = V_Wrap <$> GV.basicUnsafeFreeze mv
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeThaw (V_Wrap v) = MV_Wrap <$> GV.basicUnsafeThaw v
  {-# INLINE basicUnsafeThaw #-}
  basicLength (V_Wrap v) = GV.basicLength v
  {-# INLINE basicLength #-}
  basicUnsafeSlice s l (V_Wrap v) = V_Wrap $ GV.basicUnsafeSlice s l v
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeIndexM (V_Wrap v) i = coerce <$> GV.basicUnsafeIndexM v i
  {-# INLINE basicUnsafeIndexM #-}

instance ( UMV.Unbox (Vector d orig)
         , Coercible orig wrapped
         ) => UV.Unbox (WrapVector d orig wrapped)
