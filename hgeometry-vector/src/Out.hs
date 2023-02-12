{-# LANGUAGE UndecidableInstances #-}
module Out where

import           Control.Lens ( IxValue, conjoined, indexed, reindexed, Indexed(..)
                              , ilens, (&), (^.), (.~)
                              )
-- import           D
import           Data.Functor.Apply
import qualified Data.Vector.Generic as GV
import           Data.Vector.Generic.Mutable (MVector(basicInitialize))
import qualified Data.Vector.Generic.Mutable as GVM
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as UVM
import           GHC.Generics (Generic)
import           HGeometry.Vector.Class
import qualified In
import           R
import           Control.DeepSeq
import           System.Random (Random (..))
import           System.Random.Stateful (UniformRange(..), Uniform(..))

--------------------------------------------------------------------------------

-- | Our vector type
data Vec = Cons {-# UNPACK #-}!R
                {-# UNPACK #-}!In.Vec
  deriving stock (Eq,Ord,Generic)

type instance IxValue Vec = R

-- instance Eq Vec where
--   (Cons x r) == (Cons x' r') = x == x' && r == r'
--   {-# INLINE (==)#-}
-- instance Ord Vec where
--   (Cons x r) `compare` (Cons x' r') = x `compare` x' <> r `compare` r'
--   {-# INLINE compare #-}

instance NFData Vec

instance (IxValue In.Vec ~ R) => VectorLike_ Vec where
  components = conjoined traverse' (itraverse' . indexed)
    where
      traverse'               :: Apply f => (R -> f R) -> Vec -> f Vec
      traverse' f (Cons x r)  = Cons <$> f x <.> components f r
      itraverse'              :: Apply f => (Int -> R -> f R) -> Vec -> f Vec
      itraverse' f (Cons x r) = Cons <$> f 0 x <.> reindexed (+1) components (Indexed f) r
  {-# INLINE components #-}

  unsafeComponent i = case i of
                        0 -> ilens (\(Cons x _) -> (i, x))
                                   (\(Cons _ r) x -> Cons x r)
                        _ -> ilens (\(Cons _ r) -> (i, r^.unsafeComponent (i-1)))
                                   (\(Cons x r) y -> Cons x $ r&unsafeComponent (i-1) .~ y)
  {-# INLINE unsafeComponent  #-}
  -- not sure this will be all that efficient

instance  (IxValue In.Vec ~ R) => Additive_ Vec where
  zero = Cons 0 zero
  {-# INLINE zero #-}
  liftU2 f (Cons x r) (Cons x' r') = Cons (f x x') (liftU2 f r r')
  {-# INLINE liftU2 #-}
  liftI2 f (Cons x r) (Cons x' r') = Cons (f x x') (liftI2 f r r')
  {-# INLINE liftI2 #-}
  liftI2A f (Cons x r) (Cons x' r') = Cons <$> f x x' <*> liftI2A f r r'
  {-# INLINE liftI2A #-}

--------------------------------------------------------------------------------
-- * Unboxed vector instance

{-
-- | elements of the vector are stored consecutively
newtype instance UVM.MVector s Vec = MV_Cons (UVM.MVector s R)
newtype instance UV.Vector     Vec = V_Cons  (UV.Vector     R)

natVal' :: forall d. KnownNat d => Int
natVal' = fromInteger . natVal

instance GVM.MVector UVM.MVector Cons where
  basicLength (MV_Cons v) = let d = natVal' @D
                            in GVM.basicLength v `div` d
  {-# INLINE basicLength #-}
  basicUnsafeSlice s l (MV_Cons v) = let d = natVal' @D
                                     in MV_Cons $ GVM.basicUnsafeSlice (d*s) (d*l) v
  {-# INLINE basicUnsafeSlice #-}
  basicOverlaps  (MV_Cons v) (MV_Cons v') = GVM.basicOverlaps v v'
  {-# INLINE basicOverlaps #-}
  basicUnsafeNew n = let d = natVal' @D
                     in MV_Cons <$> GVM.basicUnsafeNew (d*n)
  {-# INLINE basicUnsafeNew #-}
  basicInitialize (MV_Cons v) = GVM.basicInitialize v
  {-# INLINE basicInitialize#-}
  basicUnsafeRead (MV_Cons v) i = let d = natVal' @D
                                  in do x <- GVM.basicUnsafeRead v (2*i)
                                        y <- GVM.basicUnsafeRead v (2*i+1)
                                        pure $ Cons x y
  {-# INLINE basicUnsafeRead #-}
  basicUnsafeWrite (MV_Cons v) i (Cons x y) = do GVM.basicUnsafeWrite v (2*i)   x
                                                 GVM.basicUnsafeWrite v (2*i+1) y
  {-# INLINE basicUnsafeWrite #-}


-- type instance GV.Mutable UV.Vector2

instance GV.Vector UV.Vector Vec where

  basicUnsafeFreeze (MV_Cons mv) = V_Cons <$> GV.basicUnsafeFreeze mv
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeThaw (V_Cons v) = MV_Cons <$> GV.basicUnsafeThaw v
  {-# INLINE basicUnsafeThaw #-}
  basicLength (V_Cons v) = GV.basicLength v `div` 2
  {-# INLINE basicLength #-}
  basicUnsafeSlice s l (V_Cons v) = V_Cons $ GV.basicUnsafeSlice (2*s) (2*l) v
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeIndexM (V_Cons v) i = Cons <$> GV.basicUnsafeIndexM v (2*i)
                                        <*> GV.basicUnsafeIndexM v (2*i+1)
  {-# INLINE basicUnsafeIndexM #-}

instance UV.Unbox Vec


-- newtype instance U.MVector s Vec = MV_Vec (U.MVector s R)
-- newtype instance U.Vector    Vec = V_Vec  (U.Vector    R)
--
-- instance U.IsoUnbox Vec R where
--   toURepr (Single x) = x
--   fromURepr = Single
--   {-# INLINE toURepr #-}
--   {-# INLINE fromURepr #-}
--
-- deriving via (Vec `U.As` R) instance U.Unbox R => GM.MVector U.MVector Vec
-- deriving via (Vec `U.As` R) instance U.Unbox R => G.Vector   U.Vector  Vec
-- instance U.Unbox R => U.Unbox Vec

-}


--------------------------------------------------------------------------------
-- * Random stuff

instance ( UniformRange R, UniformRange In.Vec
         ) => UniformRange Vec where
  uniformRM (Cons lowX lowR, Cons highX highR) gen = Cons <$> uniformRM (lowX, highX) gen
                                                          <*> uniformRM (lowR, highR) gen

instance ( Uniform R, Uniform In.Vec ) => Uniform Vec

instance ( Uniform R, UniformRange R, Uniform In.Vec, UniformRange In.Vec ) => Random Vec
