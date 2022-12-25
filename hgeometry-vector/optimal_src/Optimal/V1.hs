--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Vector.Optimal.V1
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Optimal implementation of 1 dimensional vectors (as a newtype).
--
--------------------------------------------------------------------------------
module Optimal.V1
  ( V1(Vector1)
  ) where

import Control.Applicative (liftA2)
import Control.Lens
import Data.Coerce
import GHC.Generics (Generic)
import HGeometry.Properties
import HGeometry.Vector.Class
import System.Random (Random (..))
import System.Random.Stateful (UniformRange(..), Uniform(..))

--------------------------------------------------------------------------------

-- | Wrapper around r's
newtype V1 r = Vector1 r
  deriving stock (Show,Eq,Ord,Generic,Functor,Foldable,Traversable)
  deriving newtype (Random)
  deriving (Applicative, Monad) via Identity

instance Field1 (V1 r) (V1 s) r s where
  _1 = lens coerce (\_ x -> coerce x)

--------------------------------------------------------------------------------

type instance Dimension (V1 r) = 1
type instance NumType (V1 r)   = r
type instance IxValue (V1 r)   = r
type instance Index   (V1 r)   = Int

instance TraversableWithIndex Int V1
instance FoldableWithIndex Int V1
instance FunctorWithIndex Int V1

instance Ixed (V1 r) where
  ix i f v@(Vector1 x) = case i of
                           0 -> Vector1 <$> f x
                           _ -> pure v

instance HasComponents (V1 r) (V1 s) where
  components = itraversed

instance Vector_ (V1 r) 1 r where
  vectorFromList = \case
    [x] -> Just $ coerce x
    _   -> Nothing
  {-# INLINE vectorFromList #-}

instance ConstructableVector_ (V1 r) 1 r where
  mkVector = Vector1

instance Additive_ (V1 r) where
  zero = pure 0
  liftU2 = liftA2
  liftI2 = liftA2

instance Metric_ (V1 r)


instance UniformRange r => UniformRange (V1 r) where
  uniformRM rng gen = Vector1 <$> uniformRM (coerce rng) gen

instance Uniform r => Uniform (V1 r) where
  uniformM gen = Vector1 <$> uniformM gen
