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
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
module Out
  ( Vector(..)
  , generateA
  , components
  , component'
  , liftU2
  , liftI2A
  ) where

import           Control.DeepSeq
import           Control.Lens
import           D
import qualified DIn
import qualified Data.Functor.Apply as Apply
import           Data.Type.Ord
import           GHC.Generics (Generic)
import           GHC.TypeNats
import           HGeometry.Vector.Class
import qualified In
import           R
-- import           System.Random (Random (..))
-- import           System.Random.Stateful (UniformRange(..), Uniform(..))

--------------------------------------------------------------------------------

-- | Our vector type
data Vector = Cons {-# UNPACK #-}!R
                   {-# UNPACK #-}!In.Vector
  deriving stock (Generic)

type instance IxValue Vector = R
-- type Dim = In.Dim + 1
-- instance (0 < d, d ~ Dim) => V_ Vector d R
instance (2 <= D, D ~ DIn.D + 1) => V_ Vector D R

-- deriving stock instance Show R => Show Vector
-- deriving stock instance Read R => Read Vector

deriving stock instance Eq R => Eq Vector
deriving stock instance Ord R => Ord Vector

instance NFData R => NFData Vector
-- instance Eq Vec where
--   (Cons x r) == (Cons x' r') = x == x' && r == r'
--   {-# INLINE (==)#-}
-- instance Ord Vec where
--   (Cons x r) `compare` (Cons x' r') = x `compare` x' <> r `compare` r'
--   {-# INLINE compare #-}


generateA   :: Applicative f => (Int -> f R) -> f Vector
generateA f = Cons <$> f 0 <*> In.generateA (\i -> f (i+1))
{-# INLINE generateA #-}

components :: IndexedTraversal1' Int Vector R
components = conjoined traverse' (itraverse' . indexed)
    where
      traverse'               :: Apply.Apply f => (R -> f R) -> Vector -> f Vector
      traverse' f (Cons x r)  = Cons <$> f x Apply.<.> In.components f r
      itraverse'              :: Apply.Apply f => (Int -> R -> f R) -> Vector -> f Vector
      itraverse' f (Cons x r) = Cons <$> f 0 x Apply.<.> reindexed (+1) In.components (Indexed f) r
{-# INLINE components #-}

component' :: Int -> IndexedTraversal' Int Vector R
component' i f (Cons x r) = case i of
                                0 -> (\x' -> Cons x' r)  <$> indexed f i x
                                _ -> (\r' -> Cons x  r') <$> In.component' (i-1) f r
{-# INLINE component'  #-}
  -- not sure this will be all that efficient

liftU2  :: (R -> R -> R) -> Vector -> Vector -> Vector
liftU2 f (Cons x r) (Cons x' r') = Cons (f x x') (In.liftU2 f r r')
{-# INLINE liftU2 #-}

liftI2A :: Apply.Apply f => (R -> R -> f R) -> Vector -> Vector -> f Vector
liftI2A f (Cons x r) (Cons x' r') = Cons <$> f x x' Apply.<.> In.liftI2A f r r'
{-# INLINE liftI2A #-}
