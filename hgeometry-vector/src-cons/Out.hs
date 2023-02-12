--------------------------------------------------------------------------------
-- |
-- Module      :  Out
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Implementation of unrolled vectors. Based on an the ideas from
-- https://www.well-typed.com/blog/2019/11/unrolling-data-with-backpack/
--
--------------------------------------------------------------------------------
{-# LANGUAGE UndecidableInstances #-}
module Out where

import           Control.Lens ( IxValue, conjoined, indexed, reindexed, Indexed(..)
                              , ilens, (&), (^.), (.~)
                              )
-- import           D
import           Data.Functor.Apply
import           GHC.Generics (Generic)
import           HGeometry.Vector.Class
import qualified In
import           R
import           Control.DeepSeq
-- import           System.Random (Random (..))
-- import           System.Random.Stateful (UniformRange(..), Uniform(..))

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
  generateM f = Cons <$> f 0 <*> generateM (\i -> f (i+1))
  {-# INLINE generateM #-}
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


-- --------------------------------------------------------------------------------
-- -- * Random stuff

-- instance ( UniformRange R, UniformRange In.Vec
--          ) => UniformRange Vec where
--   uniformRM (Cons lowX lowR, Cons highX highR) gen = Cons <$> uniformRM (lowX, highX) gen
--                                                           <*> uniformRM (lowR, highR) gen

-- instance ( Uniform R, Uniform In.Vec ) => Uniform Vec

-- instance ( Uniform R, UniformRange R, Uniform In.Vec, UniformRange In.Vec ) => Random Vec
