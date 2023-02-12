{-# LANGUAGE UndecidableInstances #-}
module V1 where

import           Control.Lens
import           Data.Functor.Apply
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U
import           GHC.Generics (Generic)
import           HGeometry.Vector.Class
import           R

--------------------------------------------------------------------------------

newtype Vec = Single R
  deriving stock (Eq,Ord,Generic)

type instance IxValue   Vec = R

instance VectorLike_ Vec where
  components = conjoined traverse' (itraverse' . indexed)
    where
      -- traverse'            :: Apply f => (R -> f R) -> Vec -> f Vec
      traverse' f (Single x)  = Single <$> f x
      itraverse'              :: Apply f => (Int -> R -> f R) -> Vec -> f Vec
      itraverse' f (Single x) = Single <$> f 0 x
  {-# INLINE components #-}

  unsafeComponent i = case i of
    0 -> ilens (\(Single x) -> (i,x)) (const Single)
    _ -> error $ "V1.unsafeComponent: Index " <> show i <> " out of bounds."
  {-# INLINE unsafeComponent #-}

instance Additive_ Vec where
  zero = Single 0
  liftU2 f (Single x) (Single x') = Single (f x x')
  liftI2 f (Single x) (Single x') = Single (f x x')


--------------------------------------------------------------------------------
-- * Unboxed vector instance

newtype instance U.MVector s Vec = MV_Vec (U.MVector s R)
newtype instance U.Vector    Vec = V_Vec  (U.Vector    R)

instance U.IsoUnbox Vec R where
  toURepr (Single x) = x
  fromURepr = Single
  {-# INLINE toURepr #-}
  {-# INLINE fromURepr #-}

deriving via (Vec `U.As` R) instance U.Unbox R => GM.MVector U.MVector Vec
deriving via (Vec `U.As` R) instance U.Unbox R => G.Vector   U.Vector  Vec
instance U.Unbox R => U.Unbox Vec
