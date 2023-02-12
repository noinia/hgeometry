module V1 where

import Control.Lens
import Data.Functor.Apply
import GHC.Generics (Generic)
import HGeometry.Vector.Class
import R

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
