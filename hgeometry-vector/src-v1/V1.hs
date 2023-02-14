--------------------------------------------------------------------------------
-- |
-- Module      :  V1
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Implementation of a one dimensional vector
--
--------------------------------------------------------------------------------
{-# LANGUAGE UndecidableInstances #-}
module V1 where

import           Control.DeepSeq
import           Control.Lens
import           Data.Functor.Apply
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U
import           GHC.Generics (Generic)
import           HGeometry.Vector.Class
import           R
import           System.Random (Random (..))
import           System.Random.Stateful (UniformRange(..), Uniform(..))
--------------------------------------------------------------------------------

-- | The Vector type
newtype Vec = Single R
  deriving stock (Eq,Ord,Generic)
  deriving newtype NFData

type instance IxValue   Vec = R

instance VectorLike_ Vec where
  generateA f = Single <$> f 0
  {-# INLINE generateA #-}
  components = conjoined traverse' (itraverse' . indexed)
    where
      -- traverse'            :: Apply f => (R -> f R) -> Vec -> f Vec
      traverse' f (Single x)  = Single <$> f x
      itraverse'              :: Apply f => (Int -> R -> f R) -> Vec -> f Vec
      itraverse' f (Single x) = Single <$> f 0 x
  {-# INLINE components #-}

  component' i f s@(Single x) = case i of
    0 -> Single <$> indexed f (0 :: Int) x
    _ -> pure s
  {-# INLINE component' #-}

instance Additive_ Vec where
  zero = Single 0
  {-# INLINE zero #-}
  liftU2 f (Single x) (Single x') = Single (f x x')
  {-# INLINE liftU2 #-}
  liftI2 f (Single x) (Single x') = Single (f x x')
  {-# INLINE liftI2 #-}
  liftI2A f (Single x) (Single x') = Single <$> f x x'
  {-# INLINE liftI2A #-}


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

--------------------------------------------------------------------------------

deriving newtype instance Random R => Random Vec

instance UniformRange R => UniformRange Vec where
  uniformRM (Single low, Single high) gen = Single <$> uniformRM (low,high) gen

instance Uniform R => Uniform Vec
